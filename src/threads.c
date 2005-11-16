#include <pthread.h>
#include <stdlib.h>
#include <errno.h>

#include "dm.h"
#include "threads.h"

L op_makethreads(void);
L op_threads(void);

// thread[0] is the main thread in all arrays
// Therefore, elements 0 is empty.
pthread_t threads[THREADNUM] = {};
BOOLEAN thread_start[THREADNUM] = {};
UL thread_end = 0;
UL thread_num = 1;
UL thread_max = 0;
pthread_cond_t thread_wait[THREADNUM] = {};
pthread_mutex_t thread_lock[THREADNUM] = {};
pthread_cond_t main_wait;
pthread_mutex_t main_lock;
pthread_mutex_t share_lock;
thread_func thread_function = NULL;
L thread_error[THREADNUM] = {};
const void* thread_data_global = NULL;
void* thread_data_local[THREADNUM] = {};

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
        thread_start[thread_id] = FALSE;
        do {          
          THREADERR(pthread_cond_wait, 
                    thread_wait+thread_id, 
                    thread_lock+thread_id);
        } while (! thread_start[thread_id]);

	thread_error[thread_id]
            = thread_function(thread_id, thread_data_global,
                              thread_data_local[thread_id]);
	THREADERR(pthread_mutex_lock, &main_lock);
        --thread_end;
	THREADERR(pthread_cond_signal, &main_wait);
	THREADERR(pthread_mutex_unlock, &main_lock);
  }

  pthread_cleanup_pop(1);
  pthread_cleanup_pop(1);
  pthread_cleanup_pop(1);

  return NULL;
}

L threads_do_int(UL nways, thread_func func,
                 const void* global,
                 void* local, size_t s) {
  UL i;
  if (nways > thread_num) return RNG_CHK;

  thread_max = nways-1;
  //thread_error = OK;
  thread_function = func;
  thread_data_global = global;
  if (local) for (i = nways; i--;) thread_data_local[i] = local+s*i;
  else       for (i = nways; i--;) thread_data_local[i] = NULL;

  MAINERR(pthread_mutex_lock, &main_lock);
  for (i = 1; i < nways; ++i) {
	MAINERR(pthread_mutex_lock, thread_lock+i);
        thread_start[i] = TRUE;
	MAINERR(pthread_cond_signal, thread_wait+i);
	MAINERR(pthread_mutex_unlock, thread_lock+i);
  }

  thread_error[0] = thread_function(0, thread_data_global, thread_data_local[0]);

  thread_end = thread_max;
  do {        
      MAINERR(pthread_cond_wait, &main_wait, &main_lock);
  } while (thread_end);
  MAINERR(pthread_mutex_unlock, &main_lock);

  for (i = 0; i < nways; ++i)
      if (thread_error[i]) return thread_error[i];

  return OK;
}

L threads_do_pool_int(UL nways, thread_func func, 
					  const void* global, 
					  void* local, size_t s) {
  UL i; L r; void* alloc = NULL;
  if (! local) {
	if (! (local = alloc = malloc(sizeof(UL)*nways))) return MEM_OVF;
	for (i = 0; i < nways; i++) ((UL*) local)[i] = i;
	s = sizeof(UL);
  }

  for (i = nways; i > 0; i -= thread_num, local += s*thread_num) {
	if ((r = threads_do_int((i < thread_num) ? i : thread_num, 
							func, global, local, s)) != OK) {
	  free(alloc);
	  return r;
	}
  }

  free(alloc);
  return OK;
}

L threads_destroy(L i, 
				  pthread_attr_t* attr, 
				  pthread_mutex_t* share_lock, 
				  pthread_mutex_t* main_lock,
				  pthread_cond_t* main_wait,
				  L errno_) {
  for (--i; i; --i)
	if (pthread_cancel(threads[i]) && ! errno_) 
	  errno_ = errno;

  if (attr && pthread_attr_destroy(attr) && ! errno_) 
	errno_ = errno;

  if (share_lock && pthread_mutex_destroy(share_lock))
	errno_ = errno;
  
  if (main_lock && pthread_mutex_destroy(main_lock))
	errno_ = errno;
  
  if (main_wait && pthread_cond_destroy(main_wait))
	errno_ = errno;

  return -errno_;
}

L threads_init(L num) {
  L i, errno_;
  pthread_attr_t attr;
  if (num < 1 || num > THREADNUM) return RNG_CHK;

  if (pthread_attr_init(&attr)) return -errno;
  if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED))
	return threads_destroy(0, &attr, NULL, NULL, NULL, errno);

  if (pthread_mutex_init(&share_lock, NULL))
	return threads_destroy(0, &attr, NULL, NULL, NULL, errno);
  if (pthread_mutex_init(&main_lock, NULL))
	return threads_destroy(0, &attr, &share_lock, NULL, NULL, errno);
  if (pthread_cond_init(&main_wait, NULL)) 
	return threads_destroy(0, &attr, &share_lock, &main_lock, NULL, errno);
  
  for (i = 1; i < num; ++i)
	if (pthread_create(threads+i, &attr, thread_routine, (void*) i))
	  return threads_destroy(i, &attr, &share_lock, 
							 &main_lock, &main_wait, errno);


  if (pthread_attr_destroy(&attr))
	return threads_destroy(num, NULL, NULL, NULL, NULL, errno);

  thread_num = num;
  return OK;
}

L threads_fin(void) {
  return threads_destroy(thread_num, NULL, 
						 &share_lock, &main_lock, &main_wait, 0);
}

void thread_share_unlock_f(void) {
  THREADERR(pthread_mutex_unlock, &share_lock);
}

void thread_share_lock_f(void) {
    THREADERR(pthread_mutex_lock, &share_lock);
}


/**************************************** op_threads
 *
 * -- | n
 * returns the number of threads (1 <= n <= THREADNUM)
 */
L op_threads(void) {
  if (o2 > CEILopds) return OPDS_OVF;
  TAG(o1) = NUM | LONGTYPE; ATTR(o1) = 0;
  LONG_VAL(o1) = thread_num;
  FREEopds = o2;
  return OK;
}

/**************************************** op_makethreads
 *
 * n | --
 * if n==1, destroy all threads but main
 * else destroy current threads, create n-1 new threads
 *
 */
L op_makethreads(void) {
  L n; L ret;
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! VALUE(o_1, &n)) return UNDF_VAL;

  if ((ret = (threads_fin() || threads_init(n))) == OK)
	FREEopds = o_1;
  return ret;
}
