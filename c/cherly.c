#include <stdio.h>
#include "cherly.h"
#include "common.h"

static void cherly_eject_callback(cherly_t *cherly, char *key, int length);

void cherly_init(cherly_t *cherly, int options, unsigned long long max_size) {
  cherly->hm = runtime_makemap_c(&StrMapType, max_size);
  memset(&cherly->slab, 0, sizeof(slabs_t));
  slabs_init(&cherly->slab, 0, 2, false);
  cherly->lru  = lru_create();
  cherly->size = 0;
  cherly->items_length = 0;
  cherly->max_size = max_size;
}

// node -> item -> value

void cherly_put(cherly_t *cherly, char *key, int length, void *value, int size, DestroyCallback destroy) {
  //PWord_t PValue;
  lru_item_t * item;
  String skey, sval;
  bool exists;
  size_t bufsiz = sizeof(size_t) + length + 1 + size;
  void* buf = slabs_alloc(&cherly->slab, bufsiz);
  *((size_t*)buf) = bufsiz;
  char* bufkey = (char*)((char*)buf + sizeof(size_t));
  skey.str = (byte*)bufkey;
  skey.len = length;
  memcpy(bufkey, key, length); 
  dprintf("inserting with pos %p keylen %d vallen %d\n", bufkey, length, size);
  //JHSG(PValue, cherly->judy, key, length);
  runtime_mapaccess(&StrMapType, cherly->hm, (byte*)&skey, (byte*)&sval, &exists);
  if (exists) {
    item = (lru_item_t*)sval.str;
    dprintf("removing an existing value\n");
    cherly_remove(cherly, lru_item_key(item), lru_item_keylen(item));
  }
  
  if (cherly->size + size > cherly->max_size) {
    dprintf("projected new size %lld is more than max %lld\n", cherly->size + size, cherly->max_size);
    cherly->size -= lru_eject_by_size(cherly->lru, (length + size) - (cherly->max_size - cherly->size), (EjectionCallback)cherly_eject_callback, cherly);
  }
  void* bufval = (void*)(bufkey + length + 1);
  memcpy(bufval, value, size); 
  dprintf("inserting with val'pos %p\n", bufval);
  item = lru_insert(cherly->lru, bufkey, length, bufval, size, destroy);
  
  //JHSI(PValue, cherly->judy, key, length);
  sval.str = (byte*)item;
  runtime_mapassign(&StrMapType, cherly->hm, (byte*)&skey, (byte*)&sval);
  cherly->size += lru_item_size(item);
  dprintf("new cherly size is %lld\n", cherly->size);
  cherly->items_length++;
}

void* cherly_get(cherly_t *cherly, char *key, int length, int* vallen) {
  //PWord_t PValue;
  lru_item_t * item;
  String skey, sval;
  bool exists;
  
  //JHSG(PValue, cherly->judy, key, length);
  skey.str = (byte*)key;
  skey.len = length; 
  runtime_mapaccess(&StrMapType, cherly->hm, (byte*)&skey, (byte*)&sval, &exists);
  
  if (!exists) {
    return nil;
  } else {
    item = (lru_item_t *)sval.str;
    lru_touch(cherly->lru, item);
    *vallen = lru_item_vallen(item);
    return lru_item_value(item);
  }
}

static inline void cherly_slab_free(slabs_t* slab, char* key) {
  size_t* psize = key;
  psize--;
  slabs_free(slab, (void*)psize, *psize);
}

static void cherly_eject_callback(cherly_t *cherly, char *key, int length) {
  //PWord_t PValue;
  lru_item_t *item;
  String skey, sval;
  bool exists;
  int32 ret;
  
  //JHSG(PValue, cherly->judy, key, length);
  skey.str = (byte*)key;
  skey.len = length; 
  runtime_mapaccess(&StrMapType, cherly->hm, (byte*)&skey, (byte*)&sval, &exists);
  if (!exists) {
    return;
  }
  item = (lru_item_t*)sval.str;
  cherly_slab_free(&cherly->slab, lru_item_key(item));
  
  //JHSD(ret, cherly->judy, key, length);
  ret = runtime_mapassign(&StrMapType, cherly->hm, (byte*)&skey, nil);
  if (ret) {
    cherly->items_length--;
    cherly->size -= lru_item_size(item);
  }
}

void cherly_remove(cherly_t *cherly, char *key, int length) {
  //PWord_t PValue;
  lru_item_t *item;
  String skey, sval;
  bool exists;
  
  //JHSG(PValue, cherly->judy, key, length);
  skey.str = (byte*)key;
  skey.len = length; 
  runtime_mapaccess(&StrMapType, cherly->hm, (byte*)&skey, (byte*)&sval, &exists);
  
  if (!exists) {
    return;
  }
  
  item = (lru_item_t *)sval.str;
  cherly_slab_free(&cherly->slab, lru_item_key(item));

  lru_remove_and_destroy(cherly->lru, item);
  cherly->size -= lru_item_size(item);
  cherly->items_length--;
  //JHSD(ret, cherly->judy, key, length);
  runtime_mapassign(&StrMapType, cherly->hm, (byte*)&skey, nil);
}



void cherly_destroy(cherly_t *cherly) {
  //Word_t bytes;
  //dprintf("judy %p\n", cherly->judy);
  //JHSFA(bytes, cherly->judy);
  runtime_mapdestroy(cherly->hm);
  dprintf("hashmap destroy\n");
  lru_destroy(cherly->lru);
  dprintf("lru destroy\n");
}
