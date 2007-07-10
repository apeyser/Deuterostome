#include "newfunc.h"

using namespace Plugins;
using namespace std;

size_t Allocator::prealign(void* pos) throw()
{
		
		switch (sizeof(Allocator::Node)) {
				case 0: case 1: return 0;
				case 2: return (size_t) pos % 2;
				case 3: case 4: return (4 - (size_t) pos % 4) % 4;
				default:
						return (sizeof(void*) - (size_t) pos % sizeof(void*))
								   % sizeof(void*);
		};
}

size_t Allocator::postalign(void* pos, size_t size) throw()
{
		void* next = (char*) pos + size;
		switch (sizeof(Allocator::Node)) {
				case 0: case 1: return size; break;
				case 2: return size +((size_t) next + size) % 2;
				case 3: case 4: return size + (4 - ((size_t) next + size) % 4) % 4;
				default:
						return size + (sizeof(void*) - ((size_t) next + size)
														 % sizeof(void*))
								            % sizeof(void*);
		};
}

Allocator::Allocator(void* start, size_t size) throw(bad_alloc)
{
		size_t pre = prealign(start);
		size -= pre;
		start = (char*) start + pre;
		if (size <= sizeof(Node)) throw bad_alloc();
		last = first = new(start) Node(size, NULL, false);
}


Allocator::Node* Allocator::splitNode(size_t size) throw()
{
		Node* n = first;
		while (n <= last) {
				if (n->active() || sizeof(Node) + size > n->size()) {
						n = n->next();
						continue;
				}
				
				size = postalign(n, size + sizeof(Node));
				if (size + sizeof(Node) <= n->size()) {
						Node* nn = new((char*)n+size) Node(n->size() - size, n, false);
						n->setSize(size);
						if (n == last) last = nn;
						else nn->next()->setPrev(nn);
				}
				
				return n;
		}

		return NULL;
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

void* Allocator::addNode(size_t size) throw()
{
		Node* n = splitNode(size);
		if (! n) return NULL;
		
		n->setActive(true);
		return n + sizeof(Node);
}

void* operator new(size_t size, const nothrow_t&) throw() 
{
		if (! Allocator::get()) return NULL;
		return Allocator::get()->addNode(size);
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

void Allocator::fuseNode(Node* n) throw()
{
		Node* next = n->next();
		Node* prev = n->prev();
		if (next <= last && ! next->active()) {
				n->setSize(n->size() + next->size());
				if (next == last) last = n;
				else next->next()->setPrev(n);
		}

		if (prev && ! prev->active()) {
				prev->setSize(last->size()+n->size());
				if (n == last) last = prev;
				else n->next()->setPrev(prev);
		}
}

void Allocator::removeNode(void* ptr) throw()
{
		if (! ptr) return;
		
		Node* n = (Node*) ((char*) ptr - sizeof(Node));
		n->setActive(false);
		fuseNode(n);
}

void operator delete(void* ptr, const nothrow_t&) throw() {
		if (! ptr) return;
		if (Allocator::get()) Allocator::get()->removeNode(ptr);
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
};


