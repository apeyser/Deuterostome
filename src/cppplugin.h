#ifndef CPPPLUGIN_H
#define CPPPLUGIN_H

#if __cplusplus

#include <memory>

namespace Plugins
{
		using namespace std;

		// To simply the interface to C, here we define some template
		// -- functions and macros
		// -- They map from int func(...) to void Class::func(...)
		// -- The return of func(...) will be one of MEM_ERRS,
		// -- which are the result of catching bad_alloc from new
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
		
	  enum MEM_ERRS {OK = 0, BAD_ALLOC, EXCEPTION, UNKNOWN};

	  template<void func(void)> 
	  int wrapper(void) {
	    try {func();}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception&) {return EXCEPTION;}
			catch (...) {return UNKNOWN;};
	    return OK;
	  }
                
	  template<typename A,
				     void func(A)> 
	  int wrapper1(A a) {
	    try {func(a);}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception&) {return EXCEPTION;}
			catch (...) {return UNKNOWN;};
	    return OK;
	  }

	  template<typename A, typename B,
				     void func(A, B)> 
	  int wrapper2(A a, B b) {
	    try {func(a, b);}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception&) {return EXCEPTION;}
			catch (...) {return UNKNOWN;};
	    return OK;
	  }

		template<typename A, typename B, typename C,
				     void func(A, B, C)> 
	  int wrapper3(A a, B b, C c) {
	    try {func(a, b, c);}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception&) {return EXCEPTION;}
			catch (...) {return UNKNOWN;};
	    return OK;
	  }

		template<typename A, typename B, typename C, typename D,
				     void func(A, B, C, D)> 
		int wrapper3(A a, B b, C c, D d) {
			try {func(a, b, c, d);}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception&) {return EXCEPTION;}
			catch (...) {return UNKNOWN;};
	    return OK;
	  }
};

#endif //__cplusplus

#if !CPPPLUGIN_NO_WRAPPERS

#if __cplusplus
#define wrapperM(c, n) int (*n)(void) = wrapper<c::n>
#define wrapperM1(c, n, A) int (*n)(A) = wrapper1<A, c::n>
#define wrapperM2(c, n, A, B) int (*n)(A, B) = wrapper2<A, B, c::n>
#define wrapperM3(c, n, A, B, C) int (*n)(A, B, C) = wrapper3<A, B, C, c::n>
#define wrapperM4(c, n, A, B, C, D) int (*n)(A, B, C, D)	\
				= wrapper4<A, B, C, D, c::n>
#endif //__cplusplus

#define wrapperMH(n) extern int (*n)(void)
#define wrapperMHs(n, ...) extern int (*n)(__VA_ARGS__)

#endif //!CPPPLUGIN_NO_WRAPPERS

#endif // CPPPLUGIN_H
