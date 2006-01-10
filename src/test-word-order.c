#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if ! NO_ENDIAN_HDR
#include ENDIAN_HDR
#endif //NO_ENDIAN_HDR

#if ! NO_ENDIAN_HDR && __FLOAT_WORD_ORDER
#if __FLOAT_WORD_ORDER != __BYTE_ORDER
#error "Can't handle float word order __FLOAT_WORD_ORDER"
#endif //__FLOAT_WORD_ORDER
#else //! NO_ENDIAN_HDR
#warning "Confirm that float word order = word order"
#endif //! NO_ENDIAN_HDR

int main(void) {return 0;}
