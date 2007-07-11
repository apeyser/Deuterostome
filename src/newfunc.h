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

namespace Plugins
{
		using namespace std;

		// Allocator takes a chunk of buffer as defined by a d-machine,
		//   - and then creates a mechanism for allocating it via C++ new.
		// Each Allocator is a global object - the current one set statically.
		// When global new and delete are called, they are redirected
		//   - to addNode and removeNode.
		// The C interface is defined at the end of the header.
		// The associated new and delete operators are defined in
		//   - newfunc.cpp; the library should be linked into the plugin
		//   - to replace the standard operators.
		class Allocator 
		{
			private:
				// Keep us from accidentally copying an Allocator
				Allocator& operator=(const Allocator& alloc);
				Allocator(const Allocator& alloc);
				
			public:
				// Create an Allocator with a pool starting at start buffer
				//   - with up to size bytes
				Allocator(void* start, size_t size) throw();
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

#endif // NEWFUNC_H
