#include <Judy.h>
#include "erl_nif.h"
#include <stdio.h>
#include <string.h>
#include "cherly.h"
#include "common.h"

#define CHERLY_RES_TYPE "cherly_res"

// callback for load/reload
static int _cherly_nif_onload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
	ErlNifResourceFlags erf = ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER;
	ErlNifResourceType* pert = enif_open_resource_type(env, NULL, CHERLY_RES_TYPE, NULL, erf, &erf);
	if (pert == NULL) {
		dprintf("failed loading resource types\n");
		return 1;
	}
	*priv_data = (void*)pert;
	return 0;
}
int cherly_nif_onload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
	return _cherly_nif_onload(env, priv_data, load_info);
}
int cherly_nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
	return _cherly_nif_onload(env, priv_data, load_info);
}
static ERL_NIF_TERM cherly_nif_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	ErlNifUInt64 max_size;
	ERL_NIF_TERM term;
	ErlNifResourceType* pert;
	cherly_t* obj;
	if (argc < 1) {
		return enif_make_badarg(env);
	}
	if (!enif_get_uint64(env, argv[0], &max_size)) {
		return enif_make_badarg(env);
	}
	pert = (ErlNifResourceType*)enif_priv_data(env);
	obj = enif_alloc_resource(pert, sizeof(cherly_t));
	dprintf("handle %p\n", obj);
	term = enif_make_resource(env, obj);
	cherly_init(obj, 0, max_size);
	dprintf("cherly init %d\n", max_size);
	enif_release_resource(obj);
	return term;
}

static ERL_NIF_TERM cherly_nif_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	ERL_NIF_TERM atom;
	cherly_t *obj;
	ErlNifResourceType* pert;
	if (argc < 1) {
		return enif_make_badarg(env);
	}
	pert = (ErlNifResourceType*)enif_priv_data(env);
	if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
		return enif_make_badarg(env);
	}
	dprintf("handle %p\n", obj);
	cherly_destroy(obj);
	if (!enif_make_existing_atom(env, "true", &atom, ERL_NIF_LATIN1)) {
		dprintf("failed making `true` atom \n");
		return atom;
	}
	return atom;
}

static ERL_NIF_TERM cherly_nif_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	cherly_t *obj;
	char key[1024]; // restricted by URL length
	int len;
	int vallen;
	void* value;
	ErlNifResourceType* pert;
	ErlNifBinary bin;
	if (argc < 2) {
		return enif_make_badarg(env);
	}
	pert = (ErlNifResourceType*)enif_priv_data(env);
	if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
		return enif_make_badarg(env);
	}
	dprintf("handle %p\n", obj);
	len = enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1);
	if (len <= 0) {
		return enif_make_badarg(env);
	}
	dprintf("key = %s\n", key);
	value = cherly_get(obj, key, len, &vallen);
	dprintf("get value %p\n", value);
	if (!enif_alloc_binary(vallen, &bin)) {
		return enif_make_badarg(env);
	}
	memcpy(bin.data, value, vallen);
	return enif_make_binary(env, &bin);
}

static void cherly_nif_destroy(char * key, int keylen, void * value, int vallen) {
	ErlNifBinary bin;
	bin.data = value;
	bin.size = vallen;
	enif_release_binary(&bin);
}

static ERL_NIF_TERM cherly_nif_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	cherly_t *obj;
	char key[1024]; // restricted by URL length
	int len;
	ErlNifResourceType* pert;
	ErlNifBinary bin;
	ERL_NIF_TERM atom;
	if (argc < 3) {
		return enif_make_badarg(env);
	}
	pert = (ErlNifResourceType*)enif_priv_data(env);
	if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
		return enif_make_badarg(env);
	}
	dprintf("handle %p\n", obj);
	len = enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1);
	if (len <= 0) {
		return enif_make_badarg(env);
	}
	dprintf("key = %s\n", key);
	if (!enif_inspect_binary(env, argv[2], &bin)) {
		return enif_make_badarg(env);
	}
	// get bin referenced status
	if (!enif_realloc_binary(&bin, bin.size)) {
		return enif_make_badarg(env);
	}
	cherly_put(obj, key, len, bin.data, bin.size, &cherly_nif_destroy);
	if (!enif_make_existing_atom(env, "true", &atom, ERL_NIF_LATIN1)) {
		dprintf("failed making `true` atom \n");
		return atom;
	}
	return atom;
}

static ERL_NIF_TERM cherly_nif_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	cherly_t *obj;
	char key[1024]; // restricted by URL length
	int len;
	ErlNifResourceType* pert;
	ERL_NIF_TERM atom;
	if (argc < 2) {
		return enif_make_badarg(env);
	}
	pert = (ErlNifResourceType*)enif_priv_data(env);
	if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
		return enif_make_badarg(env);
	}
	dprintf("handle %p\n", obj);
	len = enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1);
	if (len <= 0) {
		return enif_make_badarg(env);
	}
	dprintf("key = %s\n", key);
	cherly_remove(obj, key, len);
	if (!enif_make_existing_atom(env, "true", &atom, ERL_NIF_LATIN1)) {
		dprintf("failed making `true` atom \n");
		return atom;
	}
	return atom;
}

static ERL_NIF_TERM cherly_nif_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	cherly_t *obj;
	ErlNifResourceType* pert;
	ErlNifUInt64 size;
	if (argc < 1) {
		return enif_make_badarg(env);
	}
	pert = (ErlNifResourceType*)enif_priv_data(env);
	if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
		return enif_make_badarg(env);
	}
	dprintf("handle %p\n", obj);
	size = cherly_size(obj);
	dprintf("cherly size %d\n", size);
	return enif_make_uint64(env, size);
}

static ERL_NIF_TERM cherly_nif_items(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	cherly_t *obj;
	ErlNifResourceType* pert;
	ErlNifUInt64 len;
	if (argc < 1) {
		return enif_make_badarg(env);
	}
	pert = (ErlNifResourceType*)enif_priv_data(env);
	if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
		return enif_make_badarg(env);
	}
	dprintf("handle %p\n", obj);
	len = cherly_items_length(obj);
	dprintf("cherly item length %d\n", len);
	return enif_make_uint64(env, len);
}

static ErlNifFunc cherly_nif_funcs[] =
{
    {"init", 1, cherly_nif_init},
    {"stop", 1, cherly_nif_stop},
    {"get" , 2, cherly_nif_get},
    {"put" , 3, cherly_nif_put},
    {"rem" , 2, cherly_nif_remove},
    {"size", 1, cherly_nif_size},
    {"len" , 1, cherly_nif_items}
};
ERL_NIF_INIT(cherly, cherly_nif_funcs, cherly_nif_onload, NULL, cherly_nif_upgrade, NULL)

