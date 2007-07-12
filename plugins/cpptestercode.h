#ifndef CPP_H
#define CPP_H

#if __cplusplus

#include <deque>
namespace Cpp 
{
		using namespace std;

		class Tester
		{
		public:
				Tester(void);
				~Tester(void);
				
				static Tester* tester;
				typedef deque<int> dq;
				dq* l;
		};
}


extern "C" 
{
#endif
		int init(void);
		int addElem(int v);
		int getElem(int* i);
		int removeElem(void);
		int resetElems(void);
		int create(void**);
		int createSized(void**, size_t);
		int destroy(void*);
		int fini(void);
#if __cplusplus
}
#endif

#endif
