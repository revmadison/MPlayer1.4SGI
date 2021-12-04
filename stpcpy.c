#include <string.h>

#ifdef __sgi
char *stpcpy(char *dest, const char *src)
{
	int len = strlen(src);
	strcpy(dest, src);
	return dest + len;
}
#endif

