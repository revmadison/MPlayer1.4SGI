#include "stdatomic.h"
/* OLD WAY
void atomic_init(atomic_int *at, int value)
{
	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);

	pthread_mutex_init(&at->mutex, &attr);
	//at->mutex = PTHREAD_MUTEX_INITIALIZER;
	at->value = value;
	__lock_release(&at->value);
	__fetch_and_add(&at->value, value);

	//at->initialized = true;
}

int atomic_load(atomic_int *at)
{
	pthread_mutex_lock(&at->mutex);
	int value = at->value;
	pthread_mutex_unlock(&at->mutex);
	return value;
}
void atomic_store(atomic_int *at, int v)
{
	pthread_mutex_lock(&at->mutex);
	at->value = v;
	pthread_mutex_unlock(&at->mutex);
}
int atomic_fetch_add(atomic_int *at, int v)
{
	pthread_mutex_lock(&at->mutex);
	int ret = at->value;
	at->value += v;
	pthread_mutex_unlock(&at->mutex);
	return ret;
}
*/

// NEW WAY
void atomic_init(atomic_int *at, int value)
{
	__lock_release(at);
	__fetch_and_add(at, value);
}

int atomic_load(atomic_int *at)
{
	return __add_and_fetch(at, 0);
}
void atomic_store(atomic_int *at, int v)
{
	__lock_release(at);
	__add_and_fetch(at, v);
}
int atomic_fetch_add(atomic_int *at, int v)
{
	return __fetch_and_add(at, v);
}

