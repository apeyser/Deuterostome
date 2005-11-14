#ifndef THREADS_H
#define THREADS_H

#ifdef ENABLE_THREADS //intentional ifdef for undef
#if DISABLE_THREAD
#undef ENABLE_THREADS
#endif //DISABLE_THREADS
#endif //ENABLE_THREADS

#if ENABLE_THREADS

/***************************************************** thread_func
 *
 * prototype for function to multi-thread.
 *
 * id = 0..threads-1, identifying the thread of execution
 * global = pointer to data shared by all threads
 * local = pointer to data for one thread
 * return = error code (usually OK)
 *
 * As long as global is read-only, no locking should be necessary
 *
 * */
typedef L (*thread_func)(UL id, const void * global, void* local);

/************************************************** threads_do_int
 *
 * Call function multi-threaded.
 *
 * nways = number of threads (Must be less than THREADMAX and thread_num)
 * func = function to call with each thread
 * global = pointer to global data to be shared among threads
 * local = an array of data (each element will be owned by a thread)
 *         array elements must equal nways
 * size_per_local = sizeof each element in local
 * return = error code. If all threads return OK, will return OK,
 *          otherwise, will return the error from the lowest numbered
 *          thread to return error != OK.
 * 
 */
L threads_do_int(UL nways, thread_func func, const void* global,
                 void* local, size_t size_per_local);

/**************************************************** threads_do_local
 * 
 * Calls threads_do_int with local array
 *
 * local: the array of local data to be split
 *        must be an array type for the macro to work
 *
 */
#define threads_do_local(nways, func, global, local) \
    threads_do_int(nways, func, (void*) global, (void*) local, sizeof(local[0]))

/*********************************************************** threads_do
 *
 * Wrapper for threads_do_int with no local data array.
 * func will have NULL passed as `local' parameter.
 * 
 */
#define threads_do(nways, func, global) \
    threads_do_int(nways, func, (void*) global, NULL, 0)

void thread_share_lock_f(void);
void thread_share_unlock_f(void);
#define thread_share_lock() thread_share_lock_f(); {
#define thread_share_unlock() }; thread_share_unlock_f();

/*********************************************************** thread_num
 * Number of threads available/activated via makethreads.
 * 1 <= thread_num <= THREADMAX
 * 
 * */
extern UL thread_num;

/*********************************************************** thread_max
 *
 * id of highest numbered thread being used.
 * Valid only from within the thread_func.
 * 
 * 0 <= thread_max < thread_num.
 * Equal to nways passed in threads_do_int
 */
extern UL thread_max;

#else //!ENABLE_THREADS

#define thread_num (1)

#endif //!ENABLE_THREADS

#endif //THREADS_H
