#ifndef THREADS_H
#define THREADS_H

#ifdef ENABLE_THREADS
#if DISABLE_THREAD
#undef ENABLE_THREADS
#endif //DISABLE_THREADS
#endif //ENABLE_THREADS

#if ENABLE_THREADS

L op_makethreads(void);
L op_threads(void);

/***************************************************** thread_func
 *
 * prototype for function to multi-thread.
 *
 * id = 0..threads-1, identifying the thread of execution
 * global = pointer to data shared by all threads
 * local = pointer to data for one thread
 *
 * As long as global is read-only, no locking should be necessary
 * 
 * */
typedef L (*thread_func)(UL id, const void * global, void* local);

L threads_do_int(UL nways, thread_func func, void* global,
                 void* local, size_t size_per_local);

#define threads_do_local(nways, func, global, local) \
    threads_do_int(nways, func, (void*) global, (void*) local, sizeof(local[0]))

#define threads_do(nways, func, global) \
    threads_do_int(nways, func, (void*) global, NULL, 0)

void thread_share_lock_f(void);
void thread_share_unlock_f(void);
#define thread_share_lock() thread_share_lock_f(); {
#define thread_share_unlock() }; thread_share_unlock_f();
    
extern UL thread_num;
extern UL thread_max;

#else //!ENABLE_THREADS

#define thread_num (1)

#endif //!ENABLE_THREADS

#endif //THREADS_H
