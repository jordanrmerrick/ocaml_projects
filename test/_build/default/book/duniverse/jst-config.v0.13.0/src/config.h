#ifndef CONFIG_H_CONFIG_H
#define CONFIG_H_CONFIG_H
#undef  JSC_ARCH_BIG_ENDIAN
#define JSC_ARCH_SIXTYFOUR
#undef  JSC_EVENTFD
#define JSC_FDATASYNC
#undef  JSC_LINUX_EXT
#define JSC_MKOSTEMP
#undef  JSC_MSG_NOSIGNAL
#define JSC_POSIX_TIMERS
#undef  JSC_PTHREAD_NP
#undef  JSC_RECVMMSG
#define JSC_RLIMIT_AS
#undef  JSC_RLIMIT_NICE
#define JSC_SO_NOSIGPIPE
#undef  JSC_THREAD_CPUTIME
#define JSC_THREAD_ID_METHOD (1)
#undef  JSC_TIMERFD
#define JSC_WORDEXP
#endif