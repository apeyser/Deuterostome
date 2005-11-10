#ifndef THREADS_H
#define THREADS_H

#if THREADS_ENABLED
typedef L (*thread_func)(UL id, B* data);

L threads_do(UL nways, thread_func func, B* data);
L op_threads(void);
extern UL thread_num;

typedef struct {
  B* sf_start;
  B* df_start;
  UL perthread;
  UL leftover;
} thread_array_data;

#endif //THREADS_ENABLED

#endif
