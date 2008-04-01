#ifndef BASIC_DEFS_H
#define BASIC_DEFS_H

#include <inttypes.h>

#if __cplusplus
extern "C" {
#endif		

#ifndef DM_SIZEOF_VOIDP_
#define DM_SIZEOF_VOIDP_ DM_DM_SIZEOF_VOIDP_
#endif //DM_SIZEOF_VOIDP_

#if DM_SIZEOF_VOIDP_ == 4
#define DM_HOST_IS_32_BIT 1
#elif DM_SIZEOF_VOIDP_ == 8
// DM_HOST_IS_32_BIT undefined
#else
#error "void* is not 32 or 64 bit: " DM_SIZEOF_VOIDP
#endif //DM_SIZEOF_VOIDP

typedef int8_t B;
typedef int16_t W;
typedef int32_t L32;
typedef int64_t L64;

typedef uint8_t UB;
typedef uint16_t UW;
typedef uint32_t UL32;
typedef uint64_t UL64;

typedef L64 LBIG;
typedef UL64 ULBIG;

#if DM_HOST_IS_32_BIT
typedef UL32 UP;
typedef L32 P;
#else
typedef UL64 UP;
typedef L64 P;
#endif // DM_HOST_IS_32_BIT

typedef W BOOLEAN;

typedef  P  (*OPER)(void);

#ifndef TRUE
#define TRUE -1
#endif
#ifndef FALSE
#define FALSE 0
#endif

typedef float S;
typedef double D;

#define BINF    ((B) 0x80)
#define WINF    ((W) 0x8000)
#define L32INF  ((L32) 0x80000000)
#define L64INF  ((L64) 0x8000000000000000)

#define BMAX   (0x7F)
#define WMAX   (0x7FFF)
#define L32MAX (0x7FFFFFFF)
// 64 bit is different - it has a max size of 53 bits to fit in
// a double for operations; on the other hand, the extra bits
// are available, so L64MAX doesn't need to reserve any bits for
// infinity.
#define L64MAX (0x1FFFFFFFFFFFFF)

#if DM_HOST_IS_32_BIT
#define PMAX L32MAX
#define PINF L32INF
#else
#define PMAX L64MAX
#define PINF L64INF
#endif //DM_IS_32_BIT

#define LBIGMAX L64MAX
#define LBIGINF L64INF

#define PACK_FRAME (sizeof(LBIG))

/*-------------------------- VM alignment ----------------------------

NOTE: all objects that can populate the D machine's workspace must
      be aligned using the macro defined here.
*/

#define DALIGN(bytes)         (((P)(bytes)+7) & ~((P) 7)) /* 8 bytes */

/* NB: Attention to moveframe & moveframes in dm2.c whenever
   framebytes is changed */
#define FRAMEBYTES           DALIGN(3*PACK_FRAME)

// This needs to be concrete without config.h included for configure
#define NAMEBYTES ((((FRAMEBYTES/sizeof(UW)-1)/3)*8)			\
		   + (((FRAMEBYTES/sizeof(UW)-1)%3) ? 2 : 0)		\
		   + (((FRAMEBYTES/sizeof(UW)-1)%3) == 2 ? 3 : 0))

#if __cplusplus
}
#endif		

#endif //BASIC_DEFS_H
