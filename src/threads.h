#ifndef THREADS_H
#define THREADS_H

#ifdef DM_ENABLE_THREADS //intentional ifdef for undef
#if DM_DISABLE_THREADS
#undef DM_ENABLE_THREADS
#ifdef DM_HAVE_CONFIG_H
#undef ENABLE_THREADS
#endif //DM_HAVE_CONFIG_H
#endif //DM_DISABLE_THREADS
#endif //DM_ENABLE_THREADS

#if DM_ENABLE_THREADS
P op_threads(void);
P op_makethreads(void);
#endif

#if DM_ENABLE_THREADS

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
typedef P (*thread_func)(UL32 id, const void * global, void* local);

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
P threads_do_int(UL32 nways, thread_func func, const void* global,
                 void* local, size_t size_per_local);

/**************************************************** threads_do_pool_int
 *
 * Just like threads_do_int, but nways can be > thread_num
 * Func is called in serialized chunks less than thread_num.
 * local is the size of nways, and gets passed in chunks of thread_num,
 *   starting with the passed local.
 *
 */
P threads_do_pool_int(UL32 nways, thread_func func, const void* global,
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
    threads_do_int(nways, func, global, local, sizeof(local[0]))

/*************************************************** threads_do_pool_local
 * Calls threads_do_pool_int with local array.
 * Just like threads_do_local, but nways > thread_num.
 */
#define threads_do_pool_local(nways, func, global, local) \\
  threads_do_pool_int(nways, func, global, local, sizeof(local[0]))

/*********************************************************** threads_do
 *
 * Wrapper for threads_do_int with no local data array.
 * func will have NULL passed as `local' parameter.
 * 
 */
#define threads_do(nways, func, global) \
    threads_do_int(nways, func, global, NULL, 0)

/********************************************************** threads_do_pool
 *
 * Wrapper for threads_do_pool with no local data array, like threads_do.
 * Since thread_id doesn't give a serialization number,
 *   in this case a `local' parameter is created of type UL*,
 *   which points to a serialization paramter (0...nways-1)
 *
 * Can return VM_OVF if not enough VM left for UL array[nways]
 */
#define threads_do_pool(nways, func, global) \
  threads_do_pool_int(nways, func, global, NULL, 0)

void thread_share_lock_f(void);
void thread_share_unlock_f(void);
#define thread_share_lock() thread_share_lock_f(); {
#define thread_share_unlock() }; thread_share_unlock_f();

/*********************************************************** thread_num
 * Number of threads available/activated via makethreads.
 * 1 <= thread_num <= THREADMAX
 * 
 * */
extern UL32 thread_num_;
DM_INLINE_STATIC UL32 thread_num(void) {return thread_num_;};

/*********************************************************** thread_max
 *
 * id of highest numbered thread being used.
 * Valid only from within the thread_func.
 * 
 * 0 <= thread_max < thread_num.
 * Equal to nways passed in threads_do_int
 */
extern UL32 thread_max_;
__attribute__ ((__unused__)) 
DM_INLINE_STATIC UL32 thread_max(void)  {return thread_max_;};

#else //!DM_ENABLE_THREADS

//minimize the number of ifdefs by testing thread_num

#define thread_num() (1)

#endif //!DM_ENABLE_THREADS

#endif //THREADS_H
