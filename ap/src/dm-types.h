#ifndef DM_TYPES_H
#define DM_TYPES_H

#if __cplusplus
extern "C" {
#endif

#ifdef __GNUC_GNU_INLINE__
#  ifndef DM_INLINE
#    define DM_INLINE \
  __attribute__ ((__unused__, __gnu_inline__)) extern inline
#  endif
#  ifndef DM_INLINE_STATIC
#    define DM_INLINE_STATIC \
  __attribute__ ((__unused__, __gnu_inline__)) static inline
#  endif  
#elif defined(DM_GNUC_VERSION) && DM_GNUC_VERSION < 40103
#  ifndef DM_INLINE
#    define DM_INLINE __attribute__ ((__unused__)) extern inline
#  endif
#  ifndef DM_INLINE_STATIC 
#    define DM_INLINE_STATIC __attribute__ ((__unused__)) static inline
#  endif
#elif DM_HAS_INLINE
#  ifndef DM_INLINE
#    define DM_INLINE __attribute__ ((__unused__)) inline
#  endif
#  ifndef DM_INLINE_STATIC
#    define DM_INLINE_STATIC __attribute__ ((__unused__)) static inline
#  endif
#else
#  define DM_INLINE __attribute__ ((__unused__)) static
#  define DM_INLINE_STATIC __attribute__ ((__unused__)) static
#endif

#if __cplusplus
}
#endif

#endif //DM_TYPES_H
