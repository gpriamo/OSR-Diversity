
/* -*- C -*- */
/*
 *  block_template.c : Generic framework for block encryption algorithms
 *
 * Written by Andrew Kuchling and others
 *
 * ===================================================================
 * The contents of this file are dedicated to the public domain.  To
 * the extent that dedication to the public domain is not available,
 * everyone is granted a worldwide, perpetual, royalty-free,
 * non-exclusive license to exercise all rights associated with the
 * contents of this file for any purpose whatsoever.
 * No rights are reserved.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * ===================================================================
 */

#include <unistd.h>
#include "pycrypto_common.h"
/* Cipher operation modes */

#define MODE_ECB 1
#define MODE_CBC 2
#define MODE_CFB 3
#define MODE_PGP 4
#define MODE_OFB 5
#define MODE_CTR 6

#define _STR(x) #x
#define _XSTR(x) _STR(x)
#define _PASTE(x,y) x##y
#define _PASTE2(x,y) _PASTE(x,y)
#ifdef IS_PY3K
#define _MODULE_NAME _PASTE2(PyInit_,MODULE_NAME)
#else
#define _MODULE_NAME _PASTE2(init,MODULE_NAME)
#endif
#define _MODULE_STRING _XSTR(MODULE_NAME)

#ifndef KEY_LEN
	#define KEY_LEN 32
#endif


int main(int argc, char* argv[]) {
	unsigned char key[KEY_LEN] = {0};
	unsigned char in[BLOCK_SIZE] = {0};
	unsigned char out[BLOCK_SIZE] = {0};
	
	read(0, key, KEY_LEN);
	for (int i = 0; i < 10; i++) {
	block_state st = {0};

	block_init(&st, key, KEY_LEN);
	block_encrypt(&st, in, out);
	block_finalize(&st);
	}
	write(1, out, BLOCK_SIZE);

  return 0;
}