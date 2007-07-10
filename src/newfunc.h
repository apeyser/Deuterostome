#ifndef NEWFUNC_H
#define NEWFUNC_H

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
		class Allocator 
		{
			private:
				// Keep us from accidentally copying an Allocator
				Allocator& operator=(const Allocator& alloc);
				Allocator(const Allocator& alloc);
				
			public:
				// Create an Allocator with a pool starting at start buffer
				// with up to size bytes
				Allocator(void* start, size_t size) throw();
				virtual ~Allocator(void) {};

				// For D-machine dictionaries.
				void**  getStart() throw() {return &start;};
				void**  getCurr()  throw() {return &curr;};
				size_t* getSize()  throw() {return &size;};

				// set the current Allocator used for new's
				//   and return previous one.
				static Allocator* set(Allocator* alloc) throw();
				// get the current Allocator used for new's
				static Allocator* get(void) throw();

				// get memory block of at least size
				void* addNode(size_t size) throw();
				// discard the memory block at ptr; must have been created
				//   by addNode
				void  removeNode(void* ptr) throw();

				// new operator for allocator; shifts start and size to
				// account for Allocator, and checks that that some size
				// is left.
				void* operator new(size_t s, void*& start, size_t& size) throw();

		protected:
				struct Node; // defined later
				// return a new Node from pool of size, with added size for Node
				Node* getMem(size_t size) throw();
				// return a new Node from the deleted pool of size, with added size
				//   for Node
				Node* splitNode(size_t size) throw();

				// The current allocator used by new's
				static Allocator* currAlloc;

				// return offset to align Nodes at pos
				static size_t prealign(void* pos, size_t size) throw();
				// return size of Node of size size, with padding added
				//   - size should already include header size
				static size_t postalign(void* pos, size_t size) throw();

				// The padded start of the pool, after Allocator.
				void* start;
				// The current position for next node.
				void* curr;
				// The max total size of the pool, after padding and Allocator.
				size_t size;

				// Our linked list of discarded but fragmented memory
				//   - and of our active blocks allocated by new's
				struct Node 
				{
						Node* n; // next node in linked-list
						Node* l; // last node in linked-list
						size_t sz; // size of node include header and padding
				    Node(size_t sz): n(NULL), l(NULL), sz(sz) {};
				} used, freed;
				// used.n - head of active ll
				// used.l - tail of active ll
				// freed.n - head of discarded ll
				// freed.l - tail of discarded ll
		};
};


// Our C interface.
extern "C" 
{
#else
#include <stdlib.h>
#endif
		// create a new Allocator at start, of size bytes.
		//   - The whole thing may get padding, then the
		//   - allocator itself gets built, then a little more
		//   - padding may be added.
		void*   makeAllocator(void* start, size_t size);
		// set the current allocator (as returned by makeAllocator)
		//   - and return any previous allocator.
		void*   setAllocator(void* alloc);
		// interface for d-machine dictionaries.
		void**  getStart();
		void**  getCurr();
		size_t* getSize();
#if __cplusplus
}
#endif

#endif // NEWFUNC_H
