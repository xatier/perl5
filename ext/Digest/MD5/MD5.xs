/* $Id: MD5.xs,v 1.37 2003/03/09 15:20:43 gisle Exp $ */

/* 
 * This library is free software; you can redistribute it and/or
 * modify it under the same terms as Perl itself.
 * 
 *  Copyright 1998-2000 Gisle Aas.
 *  Copyright 1995-1996 Neil Winton.
 *  Copyright 1991-1992 RSA Data Security, Inc.
 *
 * This code is derived from Neil Winton's MD5-1.7 Perl module, which in
 * turn is derived from the reference implementation in RFC 1321 which
 * comes with this message:
 *
 * Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
 * rights reserved.
 *
 * License to copy and use this software is granted provided that it
 * is identified as the "RSA Data Security, Inc. MD5 Message-Digest
 * Algorithm" in all material mentioning or referencing this software
 * or this function.
 *
 * License is also granted to make and use derivative works provided
 * that such works are identified as "derived from the RSA Data
 * Security, Inc. MD5 Message-Digest Algorithm" in all material
 * mentioning or referencing the derived work.
 *
 * RSA Data Security, Inc. makes no representations concerning either
 * the merchantability of this software or the suitability of this
 * software for any particular purpose. It is provided "as is"
 * without express or implied warranty of any kind.
 *
 * These notices must be retained in any copies of any part of this
 * documentation and/or software.
 */

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif

#ifndef PERL_VERSION
#    include <patchlevel.h>
#    if !(defined(PERL_VERSION) || (SUBVERSION > 0 && defined(PATCHLEVEL)))
#        include <could_not_find_Perl_patchlevel.h>
#    endif
#    define PERL_REVISION       5
#    define PERL_VERSION        PATCHLEVEL
#    define PERL_SUBVERSION     SUBVERSION
#endif

#if PERL_VERSION <= 4 && !defined(PL_dowarn)
   #define PL_dowarn dowarn
#endif

#ifdef G_WARN_ON
   #define DOWARN (PL_dowarn & G_WARN_ON)
#else
   #define DOWARN PL_dowarn
#endif

#ifdef SvPVbyte
   #if PERL_REVISION == 5 && PERL_VERSION < 7
       /* SvPVbyte does not work in perl-5.6.1, borrowed version for 5.7.3 */
       #undef SvPVbyte
       #define SvPVbyte(sv, lp) \
	  ((SvFLAGS(sv) & (SVf_POK|SVf_UTF8)) == (SVf_POK) \
     	   ? ((lp = SvCUR(sv)), SvPVX(sv)) : my_sv_2pvbyte(aTHX_ sv, &lp))

       static char *
       my_sv_2pvbyte(pTHX_ register SV *sv, STRLEN *lp)
       {
	   sv_utf8_downgrade(sv,0);
           return SvPV(sv,*lp);
       }
   #endif
#else
   #define SvPVbyte SvPV
#endif

/* Perl does not guarantee that U32 is exactly 32 bits.  Some system
 * has no integral type with exactly 32 bits.  For instance, A Cray has
 * short, int and long all at 64 bits so we need to apply this macro
 * to reduce U32 values to 32 bits at appropriate places. If U32
 * really does have 32 bits then this is a no-op.
 */
#if BYTEORDER > 0x4321 || defined(TRUNCATE_U32)
  #define TO32(x)    ((x) &  0xFFFFffff)
  #define TRUNC32(x) ((x) &= 0xFFFFffff)
#else
  #define TO32(x)    (x)
  #define TRUNC32(x) /*nothing*/
#endif

/* The MD5 algorithm is defined in terms of little endian 32-bit
 * values.  The following macros (and functions) allow us to convert
 * between native integers and such values.
 */
#undef BYTESWAP
#ifndef U32_ALIGNMENT_REQUIRED
 #if BYTEORDER == 0x1234      /* 32-bit little endian */
  #define BYTESWAP(x) (x)     /* no-op */

 #elif BYTEORDER == 0x4321    /* 32-bit big endian */
  #define BYTESWAP(x) 	((((x)&0xFF)<<24)	\
			|(((x)>>24)&0xFF)	\
			|(((x)&0x0000FF00)<<8)	\
			|(((x)&0x00FF0000)>>8)	)
 #endif
#endif

#ifndef BYTESWAP
static void u2s(U32 u, U8* s)
{
    *s++ = (U8)(u         & 0xFF);
    *s++ = (U8)((u >>  8) & 0xFF);
    *s++ = (U8)((u >> 16) & 0xFF);
    *s   = (U8)((u >> 24) & 0xFF);
}

#define s2u(s,u) ((u) =  (U32)(*s)            |  \
                        ((U32)(*(s+1)) << 8)  |  \
                        ((U32)(*(s+2)) << 16) |  \
                        ((U32)(*(s+3)) << 24))
#endif

#define MD5_CTX_SIGNATURE 200003165

/* This stucture keeps the current state of algorithm.
 */
typedef struct {
  U32 signature;   /* safer cast in get_md5_ctx() */
  U32 A, B, C, D;  /* current digest */
  U32 bytes_low;   /* counts bytes in message */
  U32 bytes_high;  /* turn it into a 64-bit counter */
  U8 buffer[128];  /* collect complete 64 byte blocks */
} MD5_CTX;


/* Padding is added at the end of the message in order to fill a
 * complete 64 byte block (- 8 bytes for the message length).  The
 * padding is also the reason the buffer in MD5_CTX have to be
 * 128 bytes.
 */
static unsigned char PADDING[64] = {
  0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/* Constants for MD5Transform routine.
 */
#define S11 7
#define S12 12
#define S13 17
#define S14 22
#define S21 5
#define S22 9
#define S23 14
#define S24 20
#define S31 4
#define S32 11
#define S33 16
#define S34 23
#define S41 6
#define S42 10
#define S43 15
#define S44 21

/* F, G, H and I are basic MD5 functions.
 */
#define F(x, y, z) ((((x) & ((y) ^ (z))) ^ (z)))
#define G(x, y, z) F(z, x, y)
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

/* ROTATE_LEFT rotates x left n bits.
 */
#define ROTATE_LEFT(x, n) (((x) << (n) | ((x) >> (32-(n)))))

/* FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
 * Rotation is separate from addition to prevent recomputation.
 */
#define FF(a, b, c, d, s, ac)                    \
 (a) += F ((b), (c), (d)) + (NEXTx) + (U32)(ac); \
 TRUNC32((a));                                   \
 (a) = ROTATE_LEFT ((a), (s));                   \
 (a) += (b);                                     \
 TRUNC32((a));

#define GG(a, b, c, d, x, s, ac)                 \
 (a) += G ((b), (c), (d)) + X[x] + (U32)(ac);    \
 TRUNC32((a));                                   \
 (a) = ROTATE_LEFT ((a), (s));                   \
 (a) += (b);                                     \
 TRUNC32((a));

#define HH(a, b, c, d, x, s, ac)                 \
 (a) += H ((b), (c), (d)) + X[x] + (U32)(ac);    \
 TRUNC32((a));                                   \
 (a) = ROTATE_LEFT ((a), (s));                   \
 (a) += (b);                                     \
 TRUNC32((a));

#define II(a, b, c, d, x, s, ac)                 \
 (a) += I ((b), (c), (d)) + X[x] + (U32)(ac);    \
 TRUNC32((a));                                   \
 (a) = ROTATE_LEFT ((a), (s));                   \
 (a) += (b);                                     \
 TRUNC32((a));


static void
MD5Init(MD5_CTX *ctx)
{
  /* Start state */
  ctx->A = 0x67452301;
  ctx->B = 0xefcdab89;
  ctx->C = 0x98badcfe;
  ctx->D = 0x10325476;

  /* message length */
  ctx->bytes_low = ctx->bytes_high = 0;
}


static void
MD5Transform(MD5_CTX* ctx, const U8* buf, STRLEN blocks)
{
#ifdef MD5_DEBUG
    static int tcount = 0;
#endif

    U32 A = ctx->A;
    U32 B = ctx->B;
    U32 C = ctx->C;
    U32 D = ctx->D;

#ifndef U32_ALIGNMENT_REQUIRED
    const U32 *x = (U32*)buf;  /* really just type casting */
#endif

    do {
	U32 a = A;
	U32 b = B;
	U32 c = C;
	U32 d = D;

#if BYTEORDER == 0x1234 && !defined(U32_ALIGNMENT_REQUIRED)
	const U32 *X = x;
        #define NEXTx  (*x++)
#else
	U32 X[16];      /* converted values, used in round 2-4 */
	U32 *uptr = X;
	U32 tmp;
 #ifdef BYTESWAP
        #define NEXTx  (tmp=*x++, *uptr++ = BYTESWAP(tmp))
 #else
        #define NEXTx  (s2u(buf,tmp), buf += 4, *uptr++ = tmp)
 #endif
#endif

#ifdef MD5_DEBUG
	if (buf == ctx->buffer)
	    fprintf(stderr,"%5d: Transform ctx->buffer", ++tcount);
	else 
	    fprintf(stderr,"%5d: Transform %p (%d)", ++tcount, buf, blocks);

	{
	    int i;
	    fprintf(stderr,"[");
	    for (i = 0; i < 16; i++) {
		fprintf(stderr,"%x,", x[i]);
	    }
	    fprintf(stderr,"]\n");
	}
#endif

	/* Round 1 */
	FF (a, b, c, d, S11, 0xd76aa478); /* 1 */
	FF (d, a, b, c, S12, 0xe8c7b756); /* 2 */
	FF (c, d, a, b, S13, 0x242070db); /* 3 */
	FF (b, c, d, a, S14, 0xc1bdceee); /* 4 */
	FF (a, b, c, d, S11, 0xf57c0faf); /* 5 */
	FF (d, a, b, c, S12, 0x4787c62a); /* 6 */
	FF (c, d, a, b, S13, 0xa8304613); /* 7 */
	FF (b, c, d, a, S14, 0xfd469501); /* 8 */
	FF (a, b, c, d, S11, 0x698098d8); /* 9 */
	FF (d, a, b, c, S12, 0x8b44f7af); /* 10 */
	FF (c, d, a, b, S13, 0xffff5bb1); /* 11 */
	FF (b, c, d, a, S14, 0x895cd7be); /* 12 */
	FF (a, b, c, d, S11, 0x6b901122); /* 13 */
	FF (d, a, b, c, S12, 0xfd987193); /* 14 */
	FF (c, d, a, b, S13, 0xa679438e); /* 15 */
	FF (b, c, d, a, S14, 0x49b40821); /* 16 */

	/* Round 2 */
	GG (a, b, c, d,  1, S21, 0xf61e2562); /* 17 */
	GG (d, a, b, c,  6, S22, 0xc040b340); /* 18 */
	GG (c, d, a, b, 11, S23, 0x265e5a51); /* 19 */
	GG (b, c, d, a,  0, S24, 0xe9b6c7aa); /* 20 */
	GG (a, b, c, d,  5, S21, 0xd62f105d); /* 21 */
	GG (d, a, b, c, 10, S22,  0x2441453); /* 22 */
	GG (c, d, a, b, 15, S23, 0xd8a1e681); /* 23 */
	GG (b, c, d, a,  4, S24, 0xe7d3fbc8); /* 24 */
	GG (a, b, c, d,  9, S21, 0x21e1cde6); /* 25 */
	GG (d, a, b, c, 14, S22, 0xc33707d6); /* 26 */
	GG (c, d, a, b,  3, S23, 0xf4d50d87); /* 27 */
	GG (b, c, d, a,  8, S24, 0x455a14ed); /* 28 */
	GG (a, b, c, d, 13, S21, 0xa9e3e905); /* 29 */
	GG (d, a, b, c,  2, S22, 0xfcefa3f8); /* 30 */
	GG (c, d, a, b,  7, S23, 0x676f02d9); /* 31 */
	GG (b, c, d, a, 12, S24, 0x8d2a4c8a); /* 32 */

	/* Round 3 */
	HH (a, b, c, d,  5, S31, 0xfffa3942); /* 33 */
	HH (d, a, b, c,  8, S32, 0x8771f681); /* 34 */
	HH (c, d, a, b, 11, S33, 0x6d9d6122); /* 35 */
	HH (b, c, d, a, 14, S34, 0xfde5380c); /* 36 */
	HH (a, b, c, d,  1, S31, 0xa4beea44); /* 37 */
	HH (d, a, b, c,  4, S32, 0x4bdecfa9); /* 38 */
	HH (c, d, a, b,  7, S33, 0xf6bb4b60); /* 39 */
	HH (b, c, d, a, 10, S34, 0xbebfbc70); /* 40 */
	HH (a, b, c, d, 13, S31, 0x289b7ec6); /* 41 */
	HH (d, a, b, c,  0, S32, 0xeaa127fa); /* 42 */
	HH (c, d, a, b,  3, S33, 0xd4ef3085); /* 43 */
	HH (b, c, d, a,  6, S34,  0x4881d05); /* 44 */
	HH (a, b, c, d,  9, S31, 0xd9d4d039); /* 45 */
	HH (d, a, b, c, 12, S32, 0xe6db99e5); /* 46 */
	HH (c, d, a, b, 15, S33, 0x1fa27cf8); /* 47 */
	HH (b, c, d, a,  2, S34, 0xc4ac5665); /* 48 */

	/* Round 4 */
	II (a, b, c, d,  0, S41, 0xf4292244); /* 49 */
	II (d, a, b, c,  7, S42, 0x432aff97); /* 50 */
	II (c, d, a, b, 14, S43, 0xab9423a7); /* 51 */
	II (b, c, d, a,  5, S44, 0xfc93a039); /* 52 */
	II (a, b, c, d, 12, S41, 0x655b59c3); /* 53 */
	II (d, a, b, c,  3, S42, 0x8f0ccc92); /* 54 */
	II (c, d, a, b, 10, S43, 0xffeff47d); /* 55 */
	II (b, c, d, a,  1, S44, 0x85845dd1); /* 56 */
	II (a, b, c, d,  8, S41, 0x6fa87e4f); /* 57 */
	II (d, a, b, c, 15, S42, 0xfe2ce6e0); /* 58 */
	II (c, d, a, b,  6, S43, 0xa3014314); /* 59 */
	II (b, c, d, a, 13, S44, 0x4e0811a1); /* 60 */
	II (a, b, c, d,  4, S41, 0xf7537e82); /* 61 */
	II (d, a, b, c, 11, S42, 0xbd3af235); /* 62 */
	II (c, d, a, b,  2, S43, 0x2ad7d2bb); /* 63 */
	II (b, c, d, a,  9, S44, 0xeb86d391); /* 64 */

	A += a;  TRUNC32(A);
	B += b;  TRUNC32(B);
	C += c;  TRUNC32(C);
	D += d;  TRUNC32(D);

    } while (--blocks);
    ctx->A = A;
    ctx->B = B;
    ctx->C = C;
    ctx->D = D;
}


#ifdef MD5_DEBUG
static char*
ctx_dump(MD5_CTX* ctx)
{
    static char buf[1024];
    sprintf(buf, "{A=%x,B=%x,C=%x,D=%x,%d,%d(%d)}",
	    ctx->A, ctx->B, ctx->C, ctx->D,
	    ctx->bytes_low, ctx->bytes_high, (ctx->bytes_low&0x3F));
    return buf;
}
#endif


static void
MD5Update(MD5_CTX* ctx, const U8* buf, STRLEN len)
{
    STRLEN blocks;
    STRLEN fill = ctx->bytes_low & 0x3F;

#ifdef MD5_DEBUG  
    static int ucount = 0;
    fprintf(stderr,"%5i: Update(%s, %p, %d)\n", ++ucount, ctx_dump(ctx),
	                                        buf, len);
#endif

    ctx->bytes_low += len;
    if (ctx->bytes_low < len) /* wrap around */
	ctx->bytes_high++;

    if (fill) {
	STRLEN missing = 64 - fill;
	if (len < missing) {
	    Copy(buf, ctx->buffer + fill, len, U8);
	    return;
	}
	Copy(buf, ctx->buffer + fill, missing, U8);
	MD5Transform(ctx, ctx->buffer, 1);
	buf += missing;
	len -= missing;
    }

    blocks = len >> 6;
    if (blocks)
	MD5Transform(ctx, buf, blocks);
    if ( (len &= 0x3F)) {
	Copy(buf + (blocks << 6), ctx->buffer, len, U8);
    }
}


static void
MD5Final(U8* digest, MD5_CTX *ctx)
{
    STRLEN fill = ctx->bytes_low & 0x3F;
    STRLEN padlen = (fill < 56 ? 56 : 120) - fill;
    U32 bits_low, bits_high;
#ifdef MD5_DEBUG
    fprintf(stderr,"       Final:  %s\n", ctx_dump(ctx));
#endif
    Copy(PADDING, ctx->buffer + fill, padlen, U8);
    fill += padlen;

    bits_low = ctx->bytes_low << 3;
    bits_high = (ctx->bytes_high << 3) | (ctx->bytes_low  >> 29);
#ifdef BYTESWAP
    *(U32*)(ctx->buffer + fill) = BYTESWAP(bits_low);    fill += 4;
    *(U32*)(ctx->buffer + fill) = BYTESWAP(bits_high);   fill += 4;
#else
    u2s(bits_low,  ctx->buffer + fill);   fill += 4;
    u2s(bits_high, ctx->buffer + fill);   fill += 4;
#endif

    MD5Transform(ctx, ctx->buffer, fill >> 6);
#ifdef MD5_DEBUG
    fprintf(stderr,"       Result: %s\n", ctx_dump(ctx));
#endif

#ifdef BYTESWAP
    *(U32*)digest = BYTESWAP(ctx->A);  digest += 4;
    *(U32*)digest = BYTESWAP(ctx->B);  digest += 4;
    *(U32*)digest = BYTESWAP(ctx->C);  digest += 4;
    *(U32*)digest = BYTESWAP(ctx->D);
#else
    u2s(ctx->A, digest);
    u2s(ctx->B, digest+4);
    u2s(ctx->C, digest+8);
    u2s(ctx->D, digest+12);
#endif
}

#ifndef INT2PTR
#define INT2PTR(any,d)	(any)(d)
#endif

static MD5_CTX* get_md5_ctx(SV* sv)
{
    if (SvROK(sv)) {
	sv = SvRV(sv);
	if (SvIOK(sv)) {
	    MD5_CTX* ctx = INT2PTR(MD5_CTX*, SvIV(sv));
	    if (ctx && ctx->signature == MD5_CTX_SIGNATURE) {
		return ctx;
            }
        }
    }
    croak("Not a reference to a Digest::MD5 object");
    return (MD5_CTX*)0; /* some compilers insist on a return value */
}


static char* hex_16(const unsigned char* from, char* to)
{
    static char *hexdigits = "0123456789abcdef";
    const unsigned char *end = from + 16;
    char *d = to;

    while (from < end) {
	*d++ = hexdigits[(*from >> 4)];
	*d++ = hexdigits[(*from & 0x0F)];
	from++;
    }
    *d = '\0';
    return to;
}

static char* base64_16(const unsigned char* from, char* to)
{
    static char* base64 =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    const unsigned char *end = from + 16;
    unsigned char c1, c2, c3;
    char *d = to;

    while (1) {
	c1 = *from++;
	*d++ = base64[c1>>2];
	if (from == end) {
	    *d++ = base64[(c1 & 0x3) << 4];
	    break;
	}
	c2 = *from++;
	c3 = *from++;
	*d++ = base64[((c1 & 0x3) << 4) | ((c2 & 0xF0) >> 4)];
	*d++ = base64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)];
	*d++ = base64[c3 & 0x3F];
    }
    *d = '\0';
    return to;
}

/* Formats */
#define F_BIN 0
#define F_HEX 1
#define F_B64 2

static SV* make_mortal_sv(const unsigned char *src, int type)
{
    STRLEN len;
    char result[33];
    char *ret;
    
    switch (type) {
    case F_BIN:
	ret = (char*)src;
	len = 16;
	break;
    case F_HEX:
	ret = hex_16(src, result);
	len = 32;
	break;
    case F_B64:
	ret = base64_16(src, result);
	len = 22;
	break;
    default:
	croak("Bad convertion type (%d)", type);
	break;
    }
    return sv_2mortal(newSVpv(ret,len));
}


/********************************************************************/

typedef PerlIO* InputStream;

MODULE = Digest::MD5		PACKAGE = Digest::MD5

PROTOTYPES: DISABLE

void
new(xclass)
	SV* xclass
    PREINIT:
	MD5_CTX* context;
    PPCODE:
	if (!SvROK(xclass)) {
	    STRLEN my_na;
	    char *sclass = SvPV(xclass, my_na);
	    New(55, context, 1, MD5_CTX);
	    context->signature = MD5_CTX_SIGNATURE;
	    ST(0) = sv_newmortal();
	    sv_setref_pv(ST(0), sclass, (void*)context);
	    SvREADONLY_on(SvRV(ST(0)));
	} else {
	    context = get_md5_ctx(xclass);
	}
        MD5Init(context);
	XSRETURN(1);

void
clone(self)
	SV* self
    PREINIT:
	MD5_CTX* cont = get_md5_ctx(self);
	char *myname = sv_reftype(SvRV(self),TRUE);
	MD5_CTX* context;
    PPCODE:
	STRLEN my_na;
	New(55, context, 1, MD5_CTX);
	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), myname , (void*)context);
	SvREADONLY_on(SvRV(ST(0)));
	memcpy(context,cont,sizeof(MD5_CTX));
	XSRETURN(1);

void
DESTROY(context)
	MD5_CTX* context
    CODE:
        Safefree(context);

void
add(self, ...)
	SV* self
    PREINIT:
	MD5_CTX* context = get_md5_ctx(self);
	int i;
	unsigned char *data;
	STRLEN len;
    PPCODE:
	for (i = 1; i < items; i++) {
	    data = (unsigned char *)(SvPVbyte(ST(i), len));
	    MD5Update(context, data, len);
	}
	XSRETURN(1);  /* self */

void
addfile(self, fh)
	SV* self
	InputStream fh
    PREINIT:
	MD5_CTX* context = get_md5_ctx(self);
	STRLEN fill = context->bytes_low & 0x3F;
	unsigned char buffer[4096];
	int  n;
    CODE:
	if (fh) {
            if (fill) {
	        /* The MD5Update() function is faster if it can work with
	         * complete blocks.  This will fill up any buffered block
	         * first.
	         */
	        STRLEN missing = 64 - fill;
	        if ( (n = PerlIO_read(fh, buffer, missing)))
	 	    MD5Update(context, buffer, n);
	        else
		    XSRETURN(1);  /* self */
	    }

	    /* Process blocks until EOF */
            while ( (n = PerlIO_read(fh, buffer, sizeof(buffer)))) {
	        MD5Update(context, buffer, n);
	    }
	}
	XSRETURN(1);  /* self */

void
digest(context)
	MD5_CTX* context
    ALIAS:
	Digest::MD5::digest    = F_BIN
	Digest::MD5::hexdigest = F_HEX
	Digest::MD5::b64digest = F_B64
    PREINIT:
	unsigned char digeststr[16];
    PPCODE:
        MD5Final(digeststr, context);
	MD5Init(context);  /* In case it is reused */
        ST(0) = make_mortal_sv(digeststr, ix);
        XSRETURN(1);

void
md5(...)
    ALIAS:
	Digest::MD5::md5        = F_BIN
	Digest::MD5::md5_hex    = F_HEX
	Digest::MD5::md5_base64 = F_B64
    PREINIT:
	MD5_CTX ctx;
	int i;
	unsigned char *data;
        STRLEN len;
	unsigned char digeststr[16];
    PPCODE:
	MD5Init(&ctx);

	if (DOWARN) {
            char *msg = 0;
	    if (items == 1) {
		if (SvROK(ST(0))) {
                    SV* sv = SvRV(ST(0));
		    if (SvOBJECT(sv) && strEQ(HvNAME(SvSTASH(sv)), "Digest::MD5"))
		        msg = "probably called as method";
		    else
			msg = "called with reference argument";
		}
	    }
	    else if (items > 1) {
		data = (unsigned char *)SvPVbyte(ST(0), len);
		if (len == 11 && memEQ("Digest::MD5", data, 11)) {
		    msg = "probably called as class method";
		}
	    }
	    if (msg) {
		char *f = (ix == F_BIN) ? "md5" :
                          (ix == F_HEX) ? "md5_hex" : "md5_base64";
	        warn("&Digest::MD5::%s function %s", f, msg);
	    }
	}

	for (i = 0; i < items; i++) {
	    data = (unsigned char *)(SvPVbyte(ST(i), len));
	    MD5Update(&ctx, data, len);
	}
	MD5Final(digeststr, &ctx);
        ST(0) = make_mortal_sv(digeststr, ix);
        XSRETURN(1);
