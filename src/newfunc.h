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
				Allocator(void* start, size_t size) throw(bad_alloc);
				virtual ~Allocator(void) {};

				// set the current Allocator used for new's
				//   - and return previous one.
				static Allocator* set(Allocator* alloc) throw();
				// get the current Allocator used for new's
				static Allocator* get(void) throw();

				// get memory block of at least size
				void* addNode(size_t size) throw();
				// discard the memory block at ptr; must have been created
				//   - by addNode
				void  removeNode(void* ptr) throw();

				// new operator for allocator; shifts start and size to
				//   - account for Allocator, and checks that that some size
				//   - is left.
				void* operator new(size_t s, void*& start, size_t& size)
						throw(bad_alloc);

		protected:
				// Our linked list of memory
				struct Node 
				{
						Node* p;   // prev node in linked-list
						size_t sz; // size of node include header and padding
						size_t a;  // actively used
						
				    Node(size_t sz, Node* p, bool active)
								:p(p), sz(sz), a(active) {};
						
						Node*  prev(void)   {return p;};
						Node*  next(void)   {return (Node*) ((char*) this + sz);};
						bool   active(void) {return a;};
						size_t size(void)   {return sz;}
						
						void   setActive(bool active) {a = active;};
						void   setSize(size_t size)   {sz = size;};
						void   setPrev(Node* prev)    {p = prev;};
				};
				// return a new Node from the deleted pool of size, with added size
				//   -  for Node
				Node* splitNode(size_t size) throw();
				// defragment around inactive Node
				void  fuseNode(Node*) throw();

				// The current allocator used by new's
				static Allocator* currAlloc;

				// return offset to align Nodes at pos
				static size_t prealign(void* pos) throw();
				// return size of Node of size size, with padding added
				//   - size should already include header size
				static size_t postalign(void* pos, size_t size) throw();

				Node* first;
				Node* last;
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
#endif
#if __cplusplus
}
#endif

#endif // NEWFUNC_H
