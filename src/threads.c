#include <pthread.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>

#include "dm.h"
#include "threads.h"

L op_makethreads(void);
L op_threads(void);

// thread[0] is the main thread in all arrays
// Therefore, elements 0 is empty.
pthread_t threads[THREADNUM] = {};
BOOLEAN thread_start[THREADNUM] = {};
UL thread_end = 0;
UL thread_num_ = 1;
UL thread_max_ = 0;
pthread_cond_t thread_wait[THREADNUM] = {};
pthread_mutex_t thread_lock[THREADNUM] = {};
thread_func thread_function = NULL;
L thread_error[THREADNUM] = {};
const void* thread_data_global = NULL;
void* thread_data_local[THREADNUM] = {};
sigset_t sigmask;

pthread_attr_t attr;
BOOLEAN attr_i = FALSE;
pthread_cond_t main_wait;
BOOLEAN main_wait_i = FALSE;
pthread_mutex_t main_lock;
BOOLEAN main_lock_i = FALSE;
pthread_mutex_t share_lock;
BOOLEAN share_lock_i = FALSE;

#define THREAD_ERROR_EXIT(func, msg, ...) do {                          \
        int err;                                                        \
	if ((err = func(__VA_ARGS__)))                                  \
            error(EXIT_FAILURE, err,                                    \
                  "%s in %s:%d with %s(%s)",                            \
                  msg, __FILE__, __LINE__, #func, #__VA_ARGS__);        \
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

  THREADERR(pthread_cond_init, thread_wait+thread_id, NULL);
  pthread_cleanup_push(thread_destroy_wait, (void*) thread_id);

  THREADERR(pthread_mutex_init, thread_lock+thread_id, NULL);
  pthread_cleanup_push(thread_destroy_lock, (void*) thread_id);

  THREADERR(pthread_mutex_lock, thread_lock+thread_id);
  pthread_cleanup_push(thread_unlock_lock, (void*) thread_id);

  THREADERR(pthread_sigmask, SIG_BLOCK, &sigmask, NULL);

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
  if (nways > thread_num()) return RNG_CHK;

  thread_max_ = nways-1;
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

  thread_error[0] = thread_function(0, thread_data_global,
                                    thread_data_local[0]);

  thread_end = thread_max();
  do {        
      MAINERR(pthread_cond_wait, &main_wait, &main_lock);
  } while (thread_end);
  MAINERR(pthread_mutex_unlock, &main_lock);

  for (i = 0; i < nways; ++i)
      if (thread_error[i]) return thread_error[i];

  return OK;
}

static L threads_do_pool_int_(UL nways, thread_func func, 
                              const void* global, 
                              void* local, size_t s) {
    UL i; L r;
    for (i = nways; i > 0; i -= thread_num()) {
	if ((r = threads_do_int((i < thread_num()) ? i : thread_num(), 
                                func, global, local, s)) != OK)
            return r;
        local += s*thread_num();
    }
    return OK;
}

L threads_do_pool_int(UL nways, thread_func func, 
                      const void* global, 
                      void* local, size_t s) {
  UL i; L r; void* alloc = NULL;
  if (! local) {
      if (! (local = alloc = malloc(sizeof(UL)*nways))) return MEM_OVF;
      for (i = nways; i--;) ((UL*) local)[i] = i;
      s = sizeof(UL);
  }
  r = threads_do_pool_int_(nways, func, global, local, s);
  free(alloc);
  return r;
}

#define THREADS_DESTROY(func, ...) do {                       \
        int _errno;                                           \
        if ((_errno = func(__VA_ARGS__))) {                   \
            if (! errno_) errno_ = _errno;                    \
        }                                                     \
    } while (0)

#define THREADS_DESTROY_TEST(p, func) do {                 \
        if (p##_i) {                                       \
            int _errno;                                    \
            if ((_errno = func(&p))) {                     \
                if (! errno_) errno_ = _errno;             \
            }                                              \
            else p##_i = FALSE;                            \
        }                                                  \
    } while (0)
        
L threads_destroy(L errno_) {
    while (thread_num_ > 1)
        THREADS_DESTROY(pthread_cancel, threads[--thread_num_]);

    THREADS_DESTROY_TEST(attr, pthread_attr_destroy);
    THREADS_DESTROY_TEST(share_lock, pthread_mutex_destroy);
    THREADS_DESTROY_TEST(main_lock, pthread_mutex_destroy);
    THREADS_DESTROY_TEST(main_wait, pthread_cond_destroy);
    
    return -errno_;
}

#define THREADS_INIT_TEST(p, func, ...) do {     \
        int _errno;                              \
        if ((_errno = func(&p, __VA_ARGS__)))    \
            return threads_destroy(_errno);      \
        p##_i = TRUE;                            \
    } while (0)

#define THREADS_INIT_TEST1(p, func) do {         \
        int _errno;                              \
        if ((_errno = func(&p)))                 \
            return threads_destroy(_errno);      \
        p##_i = TRUE;                            \
    } while(0)

#define THREADS_INIT(func, ...) do {            \
        int _errno;                             \
        if ((_errno = func(__VA_ARGS__)))       \
            return threads_destroy(_errno);      \
    } while (0)

L threads_init(L num) {
  L i;
  if (num < 1 || num > THREADNUM) return RNG_CHK;

  if (sigfillset(&sigmask)) return -errno;

  THREADS_INIT_TEST1(attr, pthread_attr_init);
  THREADS_INIT(pthread_attr_setdetachstate, &attr, PTHREAD_CREATE_DETACHED);
  THREADS_INIT_TEST(share_lock, pthread_mutex_init, NULL);
  THREADS_INIT_TEST(main_lock, pthread_mutex_init, NULL);
  THREADS_INIT_TEST(main_wait, pthread_cond_init, NULL);
  
  for (; thread_num_ < num; ++thread_num_)
      THREADS_INIT(pthread_create,
                   threads+thread_num_,
                   &attr,
                   thread_routine,
                   (void*) thread_num_);

  return OK;
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
  LONG_VAL(o1) = thread_num();
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

  if ((ret = threads_destroy(0)) == OK && (ret = threads_init(n)) == OK)
      FREEopds = o_1;
  
  return ret;
}
