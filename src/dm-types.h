/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
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
