#include "newfunc.h"

using namespace Plugins;
using namespace std;

size_t Allocator::prealign(void* pos, size_t size_n) throw()
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

size_t Allocator::postalign(void* pos, size_t size_n) throw()
{
		void* next = (char*) pos + size_n;
		switch (sizeof(Allocator::Node)) {
				case 0: case 1: return size_n; break;
				case 2: return size_n +((size_t) next + size_n) % 2;
				case 3: case 4: return size_n + (4 - ((size_t) next + size_n) % 4) % 4;
				default:
						return size_n + (sizeof(void*) - ((size_t) next + size_n)
														 % sizeof(void*))
								            % sizeof(void*);
		};
}

Allocator::Allocator(void* start_n, size_t size_n) throw()
		: start(start_n),
			size(size_n),
			curr(start),
			used(0), freed(0)
{
		size_t pre = prealign(start, size);
		size -= pre;
		start = (char*) start + pre;
}


Allocator::Node* Allocator::getMem(size_t size_n) throw()
{
		size_t size_n1 = postalign(curr, size_n + sizeof(Node));
		void* end = (char*) start + size;
		
		if ((char*) curr + size_n + sizeof(Node) > end) return NULL;
		
		void* next = curr;
		curr = (char*) curr + size_n1;
		if (curr > end) {
				curr = end;
				size_n = (char*) end - (char*) next;
		}
		else size_n = size_n1;

		return new(next) Node(size_n);
}

Allocator::Node* Allocator::splitNode(size_t size_n) throw()
{
		Node* n = freed.n;
		while (n) {
				if (size_n + sizeof(Node) <= n->sz) {
						size_t s = postalign(n, size_n + sizeof(Node));
						if (s + sizeof(Node) > n->sz) {
								if (n->l) n->l->n = n->n;
								else freed.n = n->n;
								if (n->n) n->n->l = n->l;
								else freed.l = n->l;
						}
						else {
								Node* nn = new((char*)n+s) Node(n->sz - s);
								nn->l = n->l;
								nn->n = n->n;
								if (nn->l) nn->l->n = nn;
								else freed.n = nn;
								if (nn->n) nn->n->l = nn;
								else freed.l = nn;
								n->sz = s;
						}
						return n;
				}

				n = n->n;
		}

		return NULL;
}						

Allocator* Allocator::currAlloc = NULL;
Allocator* Allocator::set(Allocator* alloc) throw() {
		Allocator* old = currAlloc;
		currAlloc = alloc;
		return old;
};

Allocator* Allocator::get(void) throw() {return currAlloc;}

void* Allocator::addNode(size_t size_n) throw()
{
		Node* n = splitNode(size_n);
		if (n) {
				if (! used.n) {
						used.l = used.n = n;
						n->l = n->n = NULL;
				}
				else if (used.n > n) {
						n->n = used.n;
						n->l = NULL;
						n->n->l = n;
						used.n = n;
				}
				else if (used.l < n) {
						n->l = used.l;
						n->n = NULL;
						n->l->n = n;
						used.l = n;
				}
				else {
						Node* h = used.n;
						while (h < n) h = h->n;
						n->l = h->l;
						n->n = h->n;
						n->l->n = n;
						n->n->l = n;
				}
				
				return (char*) n + sizeof(Node);
		}
						
		n = getMem(size_n);
		if (! used.n) used.n = n;
		if (used.l) {
				used.l->n = n;
				n->l = used.l;
		}
		used.l = n;
		return (char*) n + sizeof(Node);
}

void* operator new(size_t size) throw(bad_alloc)
{
		if (! Allocator::get()) throw bad_alloc();
		void* r = Allocator::get()->addNode(size);
		if (! r) throw bad_alloc();
		return r;
}

void* operator new(size_t size, const nothrow_t&) throw() 
{
		if (! Allocator::get()) return NULL;
		return Allocator::get()->addNode(size);
}

void* operator new[](size_t size) throw(bad_alloc) 
{
		if (! Allocator::get()) throw bad_alloc();
		void* r = Allocator::get()->addNode(size);
		if (! r) throw bad_alloc();
		return r;
}

void* operator new[](size_t size, const nothrow_t&) throw() 
{
		if (! Allocator::get()) return NULL;
		return Allocator::get()->addNode(size);
}

void Allocator::removeNode(void* ptr) throw()
{
		if (! ptr) return;
		Node* n = (Node*) ((char*) ptr - sizeof(Node));

		if (n->n) n->n->l = n->l;
		else used.l = n->l;
		if (n->l) n->l->n = n->n;
		else used.n = n->n;

		if (! freed.n) {
				freed.l = freed.n = n;
				n->l = n->n = NULL;
		}
		else if (freed.n > n) {
				n->n = freed.n;
				n->l = NULL;
				n->n->l = n;
				freed.n = n;
		}
		else if (freed.l < n) {
				n->l = freed.l;
				n->n = NULL;
				n->l->n = n;
				freed.l = n;
		}
		else {
				Node* h = freed.n;
				while (n < h) h = h->n;
				n->l = h->l;
				n->n = h;
				n->l->n = n;
				n->n->l = n;
		}

		if (n->n && (char*) n + n->sz == (char*) n->n) {
				Node* nn = n->n;
				n->sz += nn->sz;
				n->n = nn->n;
				if (nn->n) nn->n->l = n;
				else freed.l = n;
		}

		if (n->l && (char*) n->l + n->l->sz == (char*) n) {
				Node* nl = n->l;
				nl->sz += n->sz;
				nl->n = n->n;
				if (n->n) n->n->l = nl;
				else freed.l = nl;
				n = nl;
		}

		if ((char*) n + n->sz == curr) {
				curr = n;
				freed.l = n->l;
				if (freed.n == n) freed.n = NULL;
		}
}

void operator delete(void* ptr) throw() {
		if (! ptr) return;
		if (Allocator::get()) Allocator::get()->removeNode(ptr);
};

void operator delete(void* ptr, const nothrow_t&) throw() {
		delete((char*) ptr);
};
void operator delete[](void* ptr) throw() {
		delete((char*) ptr);
};
void operator delete[](void* ptr, const nothrow_t&) throw() {
		delete((char*) ptr);
};

void* Allocator::operator new(size_t s, void*& start, size_t& size) throw()
{
		void* b = (char*) start
				+ ((sizeof(void*) - ((size_t) start % sizeof(void*)))
					 % sizeof(void*));
		
		if ((char*) b + s > (char*) start + size) return NULL;
		size  = (char*) start + size - ((char*) b + s) ;
		start = (char*) b + s;
		
		return b;
}

extern "C"
{
		void* makeAllocator(void* start, size_t size)
		{
				return new(start, size) Allocator::Allocator(start, size);
		}
		
		void* setAllocator(void* alloc)
		{
				return Allocator::set((Allocator*) alloc);
		}
		
		void** getStart() {
				if (! Allocator::get()) return NULL;
				return Allocator::get()->getStart();
		}
		
		void** getCurr() {
				if (! Allocator::get()) return NULL;
				return Allocator::get()->getCurr();
		}
		
		size_t* getSize() {
				if (! Allocator::get()) return NULL;
				return Allocator::get()->getSize();
		}
};


