#include "newfunc.h"
#include "dmalloc.h"
#include <memory>

using namespace std;
using namespace Plugins;

Allocator::Allocator(void* start, size_t size) throw(bad_alloc)
		: SizeChecker(size),
			space(create_mspace_with_base(start, size, 0)),
			init_footprint(mspace_mallinfo(space).uordblks)
{}

Allocator::~Allocator(void) throw()
{
		destroy_mspace(space);
}

Allocator* Allocator::currAlloc = NULL;
Allocator* Allocator::set(Allocator* alloc)
		throw()
{
		Allocator* old = currAlloc;
		currAlloc = alloc;
		return old;
};

Allocator* Allocator::get(void) throw() {return currAlloc;}

void* Allocator::malloc(size_t size) throw()
{
		return mspace_malloc(space, size);
}

void Allocator::free(void* ptr) throw() 
{
		mspace_free(space, ptr);
}

				
void* operator new(size_t size, const nothrow_t&) throw() 
{
		if (! Allocator::get()) return NULL;
		return Allocator::get()->malloc(size);
}

void* operator new[](size_t size, const nothrow_t& t) throw() 
{
		return operator new(size, t);
}

void* operator new(size_t size) throw(bad_alloc)
{
		void* r = operator new(size, nothrow_t());
		if (! r) throw bad_alloc();
		return r;
}

void* operator new[](size_t size) throw(bad_alloc) 
{
		return operator new(size);
}


void operator delete(void* ptr, const nothrow_t&) throw() {
		if (! ptr) return;
		if (Allocator::get()) Allocator::get()->free(ptr);
};

void operator delete[](void* ptr, const nothrow_t& t) throw() {
		operator delete((char*) ptr, t);
};

void operator delete(void* ptr) throw() {
		operator delete((char*) ptr, nothrow_t());
};

void operator delete[](void* ptr) throw() {
		operator delete((char*) ptr);
};

void* Allocator::operator new(size_t s, void*& start, size_t& size)
		throw(bad_alloc)
{
		void* b = (char*) start
				+ ((sizeof(void*) - ((size_t) start % sizeof(void*)))
					 % sizeof(void*));
		
		if ((char*) b + s > (char*) start + size) throw bad_alloc();
		size  = (char*) start + size - ((char*) b + s);
		start = (char*) b + s;

		return b;
}

int Allocator::leaked(void) 
{
		return mspace_mallinfo(space).uordblks != init_footprint;
}

extern "C"
{
		void* makeAllocator(void* start, size_t size)
		{
				try {return new(start, size) Allocator::Allocator(start, size);}
				catch (bad_alloc&) {return NULL;}
		}
		
		void* setAllocator(void* alloc)
		{
				return Allocator::set((Allocator*) alloc);
		}

		int leaked(void) 
		{
				return Allocator::get()->leaked();
		}
};

#if ENABLE_CHECK_ALLOCATOR

#include <exception>
#include <ext/new_allocator.h>

namespace gnu = __gnu_cxx;

static allocator<bool> std_allocator;
static struct AllocatorChecker
{
		struct IllegalAllocator : public exception 
		{
				IllegalAllocator(void) {};
				const char* what() const throw() {
						return "The default allocator is not __gnu_cxx::new_allocator";
				}
				
		};
		
		AllocatorChecker(void) {
				if (! dynamic_cast<gnu::new_allocator<bool>*>(&std_allocator))
						throw IllegalAllocator();
		};
} checker;

#endif //ENABLE_CHECK_ALLOCATOR
