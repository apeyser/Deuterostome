#include <pthread.h>
#include <stdlib.h>
#include <errno.h>

#include "dm.h"

pthread_t threads[THREADNUM];
UL thread_num = 0;
pthread_cond_t thread_wait[THREADNUM];
pthread_mutex_t thread_lock[THREADNUM];
pthread_cond_t main_wait;
pthread_mutex_t main_lock;
thread_func thread_function = NULL;
L thread_error = OK;
B* thread_data = NULL;

#define THREAD_ERROR_EXIT(func, msg, ...) do {							\
	if (func(__VA_ARGS__))												\
	  error(EXIT_FAILURE, errno,										\
			"%s in %s:%d with %s(%s)",									\
			msg, __FILE__, __LINE__, #func, #__VA_ARGS__);				\
  } while (0)

#define THREADERR(func, ...) THREAD_ERROR_EXIT(func, "thread", __VA_ARGS__)
#define MAINERR(func, ...) THREAD_ERROR_EXIT(func, "main", __VA_ARGS__)

void thread_unlock_lock(void* arg) {
  UL thread_id = (UL) arg;
  THREADERR(pthread_mutex_unlock, thread_lock+thread_id);
}

void thread_destroy_lock(void* arg) {
  UL thread_id = (UL) arg;
  THREADERR(pthread_mutex_destroy, thread_lock+thread_id);
}

void thread_destroy_wait(void* arg) {
  UL thread_id = (UL) arg;
  THREADERR(pthread_cond_destroy, thread_wait+thread_id);
}

void* thread_routine(void* arg) {
  UL thread_id = (UL) arg;
  L ret;

  THREADERR(pthread_cond_init, thread_wait+thread_id, NULL);
  pthread_cleanup_push(thread_destroy_wait, (void*) thread_id);

  THREADERR(pthread_mutex_init, thread_lock+thread_id, NULL);
  pthread_cleanup_push(thread_destroy_lock, (void*) thread_id);

  THREADERR(pthread_mutex_lock, thread_lock+thread_id);
  pthread_cleanup_push(thread_unlock_lock, (void*) thread_id);

  while (TRUE) {
	THREADERR(pthread_cond_wait, 
			 thread_wait+thread_id, 
			 thread_lock+thread_id);

	ret = thread_function(thread_id, thread_data);
	THREADERR(pthread_mutex_lock, &main_lock);
	if (! thread_error) thread_error = ret;
	THREADERR(pthread_cond_signal, &main_wait);
	THREADERR(pthread_mutex_unlock, &main_lock);
  }

  pthread_cleanup_pop(1);
  pthread_cleanup_pop(1);
  pthread_cleanup_pop(1);

  return NULL;
}

L threads_do(UL nways, thread_func func, B* data) {
  UL i;
  if (nways > thread_num) return RNG_CHK;

  MAINERR(pthread_mutex_lock, &main_lock);

  thread_error = OK;
  thread_function = func;
  thread_data = data;

  for (i = 0; i < nways; ++i) {
	MAINERR(pthread_mutex_lock, thread_lock+i);
	MAINERR(pthread_cond_signal, thread_wait+i);
	MAINERR(pthread_mutex_unlock, thread_lock+i);
  }

  for (; nways > 0; --nways)
	MAINERR(pthread_cond_wait, &main_wait, &main_lock);

  MAINERR(pthread_mutex_unlock, &main_lock);
  return thread_error;
}

L threads_destroy(L i, pthread_attr_t* attr, L errno_) {
  for (i--; i >= 0; --i)
	if (pthread_cancel(threads[i]) && ! errno_) 
	  errno_ = errno;

  if (attr && pthread_attr_destroy(attr) && ! errno_) 
	errno_ = errno;

  return -errno_;
}

L threads_init(L num) {
  L i, errno_;
  pthread_attr_t attr;
  if (num < 0 || num > THREADNUM) return RNG_CHK;

  if (pthread_attr_init(&attr)) return -errno;
  if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED))
	return threads_destroy(0, &attr, errno);
  
  for (i = 0; i < num; ++i)
	if (pthread_create(threads+i, &attr, thread_routine, (void*) i))
	  return threads_destroy(i, &attr, errno);	


  if (pthread_attr_destroy(&attr))
	return threads_destroy(num, NULL, errno);

  thread_num = num;
  return OK;
}

L threads_fin(void) {
  return threads_destroy(thread_num, NULL, OK);
}

/**************************************** op_threads
 *
 * n | --
 * if n==0, destroy all threads
 * else destroy current threads, create n new threads
 *
 */
L op_threads(void) {
  L n;
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! VALUE(o_1, &n)) return UNDF_VAL;

  return threads_fin() || threads_init(n);
}
