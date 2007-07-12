#ifndef DMALLOC_ABORT_H
#define DMALLOC_ABORT_H

namespace Dmalloc 
{
		using namespace std;
		
		struct Abort : public bad_exception 
		{
				Abort(void) {}
				virtual const char* what(void) const throw()
						{return "Abort in dmalloc.cpp";}
		};
		
		static void abort(void) throw(Abort) {throw Abort();};
};

#endif
