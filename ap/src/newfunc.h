#ifndef NEWFUNC_H
#define NEWFUNC_H

// This header is not intended to be included by cpp code, but by c-code.
// -- newfunc.cpp includes it and redefines the global new operator
// -- for cpp code, which then affects that code by being linked in
// -- as a library.
// The plugin C code includes this header to set the pool associated with
// -- each opaque object which will be used by associated C functions that
// -- then call into a cpp library.
// The calls for C are makeAllocator and setAllocator.

#if __cplusplus
#include <new>
#include "dmalloc-abort.h"

namespace Plugins
{
		using namespace std;
		using namespace Dmalloc;

		class SizeChecker 
		{
			protected:
				SizeChecker(size_t size) throw (bad_alloc) {
						if (size <= 128*sizeof(size_t)) throw bad_alloc();
				};
		};
		

		// Allocator takes a chunk of buffer as defined by a d-machine,
		//   - and then creates a mechanism for allocating it via C++ new.
		// Each Allocator is a global object - the current one set statically.
		// When global new and delete are called, they are redirected
		//   - to addNode and removeNode.
		// The C interface is defined at the end of the header.
		// The associated new and delete operators are defined in
		//   - newfunc.cpp; the library should be linked into the plugin
		//   - to replace the standard operators.
		class Allocator : protected SizeChecker
		{
			private:
				// Keep us from accidentally copying an Allocator
				Allocator& operator=(const Allocator& alloc);
				Allocator(const Allocator& alloc);
				
			public:
				// Create an Allocator with a pool starting at start buffer
				//   - with up to size bytes
				Allocator(void* start, size_t size) throw(bad_alloc);
				virtual ~Allocator(void) throw();

				// set the current Allocator used for new's
				//   - and return previous one.
				static Allocator* set(Allocator* alloc) throw();
				// get the current Allocator used for new's
				static Allocator* get(void) throw();
				
				// new operator for allocator; shifts start and size to
				//   - account for Allocator, and checks that that some size
				//   - is left.
				void* operator new(size_t s, void*& start, size_t& size)
						throw(bad_alloc);

				int leaked(void);

				void* malloc(size_t) throw();
				void  free(void*) throw();

		protected:
				// The current allocator used by new's
				static Allocator* currAlloc;

				void* space;
				size_t init_footprint;
		};

		// To simply the interface to C, here we define some template
		// -- functions and macros
		// -- They map from int func(...) to void Class::func(...)
		// -- The return of func(...) will be one of MEM_ERRS,
		// -- which are the result of catching bad_alloc from new
		// -- or Abort from dmalloc.
		// -- So, you declare int (*func)(...) in the header
		// -- and set it equal to wrapper|N|<..., Class::func(...)>
		// -- in the body,
		// -- then define static void Class::func(...)
		// -- without worrying about the catches
		// The macros then simplify making these definitions.
		// -- In the header, you call wrapperMH(func)
		// -- or wrapperMH(func, ...)
		// -- and then match it in the implementation file
		// -- with wrapperM|N|(class, func, ...)
		// -- and of course a definition for static void class::func(...)
		
	  enum MEM_ERRS {OK = 0, BAD_ALLOC, ABORT_ALLOC};

	  template<void func(void)> 
	  int wrapper(void) {
	    try {func();}
	    catch (bad_alloc&) {return BAD_ALLOC;}
	    catch (Abort&)     {return ABORT_ALLOC;};
	    return OK;
	  }
#define wrapperM(c, n) int (*n)(void) = wrapper<c::n>
                
	  template<typename A,
				     void func(A)> 
	  int wrapper1(A a) {
	    try {func(a);}
	    catch (bad_alloc&) {return BAD_ALLOC;}
	    catch (Abort&)     {return ABORT_ALLOC;};
	    return OK;
	  }
#define wrapperM1(c, n, A) int (*n)(A) = wrapper1<A, c::n>

	  template<typename A, typename B,
				     void func(A, B)> 
	  int wrapper2(A a, B b) {
	    try {func(a, b);}
	    catch (bad_alloc&) {return BAD_ALLOC;}
	    catch (Abort&)     {return ABORT_ALLOC;};
	    return OK;
	  }
#define wrapperM2(c, n, A, B) int (*n)(A, B) = wrapper2<A, B, c::n>

		template<typename A, typename B, typename C,
				     void func(A, B, C)> 
	  int wrapper3(A a, B b, C c) {
	    try {func(a, b, c);}
	    catch (bad_alloc&) {return BAD_ALLOC;}
	    catch (Abort&)     {return ABORT_ALLOC;};
	    return OK;
	  }
#define wrapperM3(c, n, A, B, C) int (*n)(A, B, C) = wrapper3<A, B, C, c::n>

		template<typename A, typename B, typename C, typename D,
				     void func(A, B, C, D)> 
		int wrapper3(A a, B b, C c, D d) {
			try {func(a, b, c, d);}
	    catch (bad_alloc&) {return BAD_ALLOC;}
	    catch (Abort&)     {return ABORT_ALLOC;};
	    return OK;
	  }
#define wrapperM4(c, n, A, B, C, D) int (*n)(A, B, C, D)	\
				= wrapper4<A, B, C, D, c::n>
};

// Our C interface
extern "C" 
{
		// the versions for cpp compiler
		void*   makeAllocator(void* start, size_t size);
		void*   setAllocator(void* alloc);
#else
#include <stdlib.h>
		// a little sugar for the plugin library...
		typedef struct Allocator {} Allocator;
		// create a new Allocator at start, of size bytes.
		//   - The whole thing may get padding, then the
		//   - allocator itself gets built, then a little more
		//   - padding may be added.
		Allocator*   makeAllocator(void* start, size_t size);
		// set the current allocator (as returned by makeAllocator)
		//   - and return any previous allocator.
		Allocator*   setAllocator(Allocator* alloc);
		int leaked(void);
#endif
#if __cplusplus
}
#endif

#if !NEWFUNC_NO_WRAPPERS
#define wrapperMH(n) extern int (*n)(void)
#define wrapperMHs(n, ...) extern int (*n)(__VA_ARGS__)
#endif //!NEWFUNC_NO_WRAPPERS

#endif // NEWFUNC_H
