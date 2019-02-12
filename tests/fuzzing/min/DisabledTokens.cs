#define check

#if what
this is garbage text
#elif check
#if the_heck
this is also garbage
#else
#if check
class Foo {}
#elif not_here
nope no text here
#elif check
Hey this should be skipped because it's in an 'elif''!!
#else
what is this nonsense
#endif
#endif
#endif
