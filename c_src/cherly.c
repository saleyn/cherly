#include <stdio.h>
#include <string.h>
#include "cherly.h"
#include "common.h"

static void cherly_eject_callback(cherly_t *cherly, char *key, int length);

/**
 * Initialize LRU-Storage
 */
void cherly_init(cherly_t *cherly, int options, unsigned long long max_size) {
  cherly->hm = runtime_makemap_c(&StrMapType, max_size);
  memset(&cherly->slab, 0, sizeof(slabs_t));
  slabs_init(&cherly->slab, 0, 2, false);

  cherly->lru  = lru_create();
  cherly->size = 0;
  cherly->items_length = 0;
  cherly->max_size = max_size;
}


/**
 * Insert an object into LRU-Storage
 */
// node -> item -> value
void cherly_put(cherly_t *cherly, void *key, int length, void *value, int size, DestroyCallback destroy) {
  lru_item_t * item;
  String skey, sval;
  bool exists;

  // Prepare put-operation
  size_t bufsiz = sizeof(size_t) + length + 1 + size;
  void* buf = slabs_alloc(&cherly->slab, bufsiz);
  *((size_t*)buf) = bufsiz;
  char* bufkey = (char*)((char*)buf + sizeof(size_t));

  skey.str = (byte*)bufkey;
  skey.len = length;

  memcpy(bufkey, key, length);
  runtime_mapaccess(&StrMapType, cherly->hm, (byte*)&skey, (byte*)&sval, &exists);

  if (exists) {
    item = (lru_item_t*)sval.str;
    cherly_remove(cherly, lru_item_key(item), lru_item_keylen(item));
  }

  if (cherly->size + size > cherly->max_size) {
    cherly->size -= lru_eject_by_size(cherly->lru,
                                      (length + size) - (cherly->max_size - cherly->size),
                                      (EjectionCallback)cherly_eject_callback, cherly);
  }

  void* bufval = (void*)(bufkey + length + 1);
  memcpy(bufval, value, size);

  // Insert an object into lru-storage
  item = lru_insert(cherly->lru, bufkey, length, bufval, size, destroy);

  // After put-operation
  sval.str = (byte*)item;
  runtime_mapassign(&StrMapType, cherly->hm, (byte*)&skey, (byte*)&sval);

  cherly->size += lru_item_size(item);
  cherly->items_length++;
}


/**
 * Retrieve an object from LRU-Storage
 */
void* cherly_get(cherly_t *cherly, void *key, int length, int* vallen) {
  lru_item_t * item;
  String skey, sval;
  bool exists;

  // Prepare get-operation
  skey.str = (byte*)key;
  skey.len = length;

  // Retrieve an object
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


/**
 * Free a stored memory
 */
static inline void cherly_slab_free(slabs_t* slab, char* key) {
  size_t* psize = (size_t*)key;
  psize--;
  slabs_free(slab, (void*)psize, *psize);
}


/**
 * Callback
 */
static void cherly_eject_callback(cherly_t *cherly, char *key, int length) {
  lru_item_t *item;
  String skey, sval;
  bool exists;
  int32 ret;

  skey.str = (byte*)key;
  skey.len = length;
  runtime_mapaccess(&StrMapType, cherly->hm, (byte*)&skey, (byte*)&sval, &exists);

  if (!exists) {
    return;
  }

  item = (lru_item_t*)sval.str;
  cherly_slab_free(&cherly->slab, lru_item_key(item));
  ret = runtime_mapassign(&StrMapType, cherly->hm, (byte*)&skey, nil);

  if (ret) {
    cherly->items_length--;
    cherly->size -= lru_item_size(item);
  }
}


/**
 * Remove an object from LRU-Storage
 */
void cherly_remove(cherly_t *cherly, void *key, int length) {
  lru_item_t *item;
  String skey, sval;
  bool exists;


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

  runtime_mapassign(&StrMapType, cherly->hm, (byte*)&skey, nil);
}


/**
 * Destroy LRU-Storage
 */
void cherly_destroy(cherly_t *cherly) {
  runtime_mapdestroy(cherly->hm);
  lru_destroy(cherly->lru);
}

