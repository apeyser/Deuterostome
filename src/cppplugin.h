#ifndef CPPPLUGIN_H
#define CPPPLUGIN_H

#if __cplusplus

#include <memory>
#include <iostream>

namespace Plugins
{
		using namespace std;

		class Uncopyable 
		{
		private:
				Uncopyable& operator=(const Uncopyable&);
				Uncopyable(const Uncopyable&);
		public:
				Uncopyable(void) {};
		};

#ifdef DM_H
		struct Frame 
		{
				typedef B bftype[FRAMEBYTES];
				bftype bframe;

				Frame(const bftype f) {moveframe((B*)f, bframe);};
				void getframe(bftype f) const {moveframe((B*)bframe, f);};
		};

		struct ValFrame: public Frame
		{
				ValFrame(const bftype f): Frame(f) {};
				char* ptr(void) const {return (char*) VALUE_PTR(bframe);};
		};

		struct ArrFrame: public ValFrame 
		{
				ArrFrame(const bftype f): ValFrame(f) {};
				size_t getlen(void) const {return ARRAY_SIZE(bframe);};
				void setlen(size_t s) {ARRAY_SIZE(bframe) = s;};
		};

		typedef ArrFrame StringFrame;

		struct NameFrame: public Frame
		{
				NameFrame(const bftype f): Frame(f) {};
				string str(void) const {
						B s[DM_NAMEBYTES+1];
						pullname((B*) bframe, s);
						return string((char*) s);
				};

				
				typedef bool (*Comparator)(const NameFrame&, const NameFrame&);
				static bool comparator(const NameFrame& a, const NameFrame& b) {
						return compname((B*) a.bframe, (B*) b.bframe) < 0;
				};
		};

#endif // DM_H
								

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
		
	  enum MEM_ERRS {NO_ERR = 0, BAD_ALLOC, EXCEPTION, UNKNOWN};

		class ErrorCode : public exception 
		{
				const char *const whatptr;
				const int err;

		public:
				const char* what(void) {return whatptr;};
				int code(void) {return err;};
				
				ErrorCode(int err_, const char* whatptr_)
						: err(err_), whatptr(whatptr_) {};
		};

		template<const int e, const char* w>
		class ErrorCodeInst : public ErrorCode 
		{
		public:
				ErrorCodeInst(void): ErrorCode(e, w) {};
		};
		

	  template<void func(void)> 
	  int wrapper(void) {
	    try {func();}
			catch (ErrorCode& e) {return e.code();}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception& e) {
					cout << "Standard exception: " << e.what() << endl;
					return EXCEPTION;
			}
			catch (...) {return UNKNOWN;};
	    return NO_ERR;
	  }
                
	  template<typename A,
				     void func(A)> 
	  int wrapper1(A a) {
	    try {func(a);}
			catch (ErrorCode& e) {return e.code();}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception& e) {
					cout << "Standard exception: " << e.what() << endl;
					return EXCEPTION;
			}
			catch (...) {return UNKNOWN;};
	    return NO_ERR;
	  }

	  template<typename A, typename B,
				     void func(A, B)> 
	  int wrapper2(A a, B b) {
	    try {func(a, b);}
			catch (ErrorCode& e) {return e.code();}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception& e) {
					cout << "Standard exception: " << e.what() << endl;
					return EXCEPTION;
			}
			catch (...) {return UNKNOWN;};
	    return NO_ERR;
	  }

		template<typename A, typename B, typename C,
				     void func(A, B, C)> 
	  int wrapper3(A a, B b, C c) {
	    try {func(a, b, c);}
			catch (ErrorCode& e) {return e.code();}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception& e) {
					cout << "Standard exception: " << e.what() << endl;
					return EXCEPTION;
			}
			catch (...) {return UNKNOWN;};
	    return NO_ERR;
	  }

		template<typename A, typename B, typename C, typename D,
				     void func(A, B, C, D)> 
		int wrapper4(A a, B b, C c, D d) {
			try {func(a, b, c, d);}
			catch (ErrorCode& e) {return e.code();}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception& e) {
					cout << "Standard exception: " << e.what() << endl;
					return EXCEPTION;
			}
			catch (...) {return UNKNOWN;};
	    return NO_ERR;
	  }

		template<typename A, typename B, typename C, typename D, typename E,
				     void func(A, B, C, D, E)> 
		int wrapper5(A a, B b, C c, D d, E e) {
			try {func(a, b, c, d, e);}
			catch (ErrorCode& e) {return e.code();}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception& e) {
					cout << "Standard exception: " << e.what() << endl;
					return EXCEPTION;
			}
			catch (...) {return UNKNOWN;};
	    return NO_ERR;
	  }

		template<typename A, typename B, typename C, typename D, typename E,
						 typename F, void func(A, B, C, D, E, F)> 
		int wrapper6(A a, B b, C c, D d, E e, F f) {
			try {func(a, b, c, d, e, f);}
			catch (ErrorCode& e) {return e.code();}
	    catch (bad_alloc&) {return BAD_ALLOC;}
			catch (exception& e) {
					cout << "Standard exception: " << e.what() << endl;
					return EXCEPTION;
			}
			catch (...) {return UNKNOWN;};
	    return NO_ERR;
	  }
};

#endif //__cplusplus

#if !CPPPLUGIN_NO_WRAPPERS

#if __cplusplus
#define wrapperM(c, n) int (*n)(void) = wrapper<c::n>
#define wrapperMs(i, c, n, ...) int (*n)(__VA_ARGS__) \
		= wrapper##i<__VA_ARGS__, c::n>
#endif //__cplusplus

#define wrapperMH(c, n) extern int (*n)(void)
#define wrapperMHs(i, c, n, ...) extern int (*n)(__VA_ARGS__)

#define wrapperECI(name, plugin, err)							 \
		char name##_Msg[] = "Plugin: " #plugin ", Error: " #name ", Number: " #err;	\
		typedef ErrorCodeInst<plugin##_##err, name##_Msg> name

#endif //!CPPPLUGIN_NO_WRAPPERS

#endif // CPPPLUGIN_H
