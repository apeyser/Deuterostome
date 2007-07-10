#ifndef NEWFUNC_H
#define NEWFUNC_H

#if __cplusplus
#include <new>

namespace Plugins
{
		using namespace std;

		class Allocator 
		{
			private:
				Allocator& operator=(const Allocator& alloc);
				Allocator(const Allocator& alloc);
				
			public:
				Allocator(void* start, size_t size) throw();
				virtual ~Allocator(void) {};
				
				void**  getStart() throw() {return &start;};
				void**  getCurr()  throw() {return &curr;};
				size_t* getSize()  throw() {return &size;};
				
				static Allocator* set(Allocator* alloc) throw();
				static Allocator* get(void) throw();
				
				void* addNode(size_t size) throw();
				void  removeNode(void* ptr) throw();

				void* operator new(size_t s, void*& start, size_t& size) throw();

		protected:
				struct Node;
				Node* getMem(size_t size) throw();
				Node* splitNode(size_t size) throw();
				
				static Allocator* currAlloc;

				static size_t prealign(void* pos, size_t size) throw();
				static size_t postalign(void* pos, size_t size) throw();

				void* start;
				void* curr;
				size_t size;

				struct Node 
				{
						Node* n;
						Node* l;
						size_t sz;
				    Node(size_t sz): n(NULL), l(NULL), sz(sz) {};
				} used, freed;
		};
};

extern "C" 
{
#else
#include <stdlib.h>
#endif
		void*   makeAllocator(void* start, size_t size);
		void*   setAllocator(void* alloc);
		void**  getStart();
		void**  getCurr();
		size_t* getSize();
#if __cplusplus
}
#endif

#endif // NEWFUNC_H
