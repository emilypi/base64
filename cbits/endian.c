#include <stdint.h>


#define BASE64_HTOBE64(x) __builtin_bswap64(x)


uint64_t swap64(uint64_t a)
{
	return BASE64_HTOBE64(a);
}
