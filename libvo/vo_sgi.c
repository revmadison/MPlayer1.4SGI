#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <unistd.h>
#include <invent.h>

#include "mp_msg.h"
#include "config.h"
#include "video_out.h"
#define NO_DRAW_FRAME
#include "video_out_internal.h"
#include "sub/sub.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <pthread.h>
#include <semaphore.h>
#include <errno.h>

#include <GL/gl.h>

#include <dmedia/dmedia.h>
#include <dmedia/dm_image.h>
#include <dmedia/dm_buffer.h>

#include <GL/glx.h>

#include "x11_common.h"
#include "aspect.h"

#include "sgi_unpack.h"

#ifdef HAVE_NEW_GUI
#include "Gui/interface.h"
#endif

static vo_info_t info = 
{
	"X11 (OpenGL/SGI)",
	"sgi",
	"SGUG",
	""
};

LIBVO_EXTERN(sgi)

static XVisualInfo *gl_vinfo = NULL;
static GLXContext gl_context = 0;

struct TexSquare
{
	GLubyte *texture;
	GLuint texobj;
	int isTexture;
	GLfloat fx1, fy1, fx2, fy2, fx3, fy3, fx4, fy4;
	GLfloat xcov, ycov;
	int isDirty;
	int dirtyXoff, dirtyYoff, dirtyWidth, dirtyHeight;
};
static int texnumx, texnumy, raw_line_len;
static GLfloat texpercx, texpercy;
static struct TexSquare * texgrid;

enum {
	HW_ENTRY,
	HW_EXPRESS,
	HW_O2,
	HW_MGRAS,
	HW_VPRO,
	HW_IR,
	HW_UV,
	HW_UNKNOWN
};
static int hw_detected;

static GLXFBConfig *fbConfigs;
static GLXPbufferSGIX _dmpbuffer;
static GLXContext _pbcontext;
static DMbuffer       _gfxDMBuffer;    /* The DMBuffer that provides frame buffer memory for _dmpbuffer */
static DMbufferpool   _dmpool;		/* The memory pool _gfxDMBuffer is allocated from .		  */	
static DMparams      *_dmParams;       /* Params describing _dmpool characteristics.		          */

static uint32_t clearcounter;

static uint32_t has_dcd = 0;

static uint32_t use_rendertotex, force_rendertotex;
static uint32_t use_softcs, force_softcs;
static uint32_t use_pixeltex, force_pixeltex;
static uint32_t use_ycrcb, force_ycrcb;
static uint32_t use_colormatrix, force_colormatrix;
static uint32_t use_drawpixels, force_drawpixels;
static uint32_t use_textures, force_textures;
static uint32_t use_rgba, force_rgba;
static uint32_t use_rgb, force_rgb;
static uint32_t use_abgr, force_abgr;
static uint32_t use_async, force_async;

static uint32_t async_marker, async_first;

static uint8_t *imagedata;

static uint32_t image_width;
static uint32_t image_bytes;
static uint32_t image_height;
static uint32_t image_format;

static uint32_t texture_width;
static uint32_t texture_height;

static int paused;
static int gl_antialias=0;
static float xscale, yscale;

static int skip_frame;
static int frame_skip_counter;
static int frame_skip;

static int double_buffer = 1;

static void resize(int *x, int *y){
	clearcounter = 2;

	if(vo_fs) {
		glClear(GL_COLOR_BUFFER_BIT);
		aspect(x, y, A_ZOOM);
		panscan_calc();
		*x += vo_panscan_x;
		*y += vo_panscan_y;
		glViewport((vo_screenwidth-*x)/2, (vo_screenheight-*y)/2, *x, *y);
	} else {
		if (WinID >= 0) {
			int top = 0, left = 0, w = *x, h = *y;
			geometry(&top, &left, &w, &h, vo_screenwidth, vo_screenheight);
			glViewport(top, left, w, h);
		} else {
			glViewport( 0, 0, *x, *y );
		}
	}

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho (0, 1, 1, 0, -1.0, 1.0);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	if(image_width && image_height){
		xscale = ((float)*x)/((float)image_width);
		yscale = -((float)*y)/((float)image_height);
	} else {
		xscale = 1.0f;
		yscale = -1.0f;
	}

	mp_msg(MSGT_VO, MSGL_V, "[sgi] Resize: %dx%d Scale: %fx%f\n",*x,*y,xscale,yscale);
}

static void resetTexturePointers(unsigned char *imageSource)
{
	unsigned char *texdata_start, *line_start;
	struct TexSquare *tsq = texgrid;
	int x=0, y=0;

	if(use_rgb) image_bytes = 3;
	else if(use_rgba || use_abgr) image_bytes = 4;
	else image_bytes = 4;

	line_start = (unsigned char *) imageSource;

	for(y = 0; y < texnumy; y++) {
		texdata_start = line_start;
		for(x = 0; x < texnumx; x++) {
			tsq->texture = texdata_start;
			texdata_start += texture_width * image_bytes;
			tsq++;
		}
		line_start += texture_height * raw_line_len;
	}
}

static void setupTextureDirtyArea(int x, int y, int w,int h)
{
	struct TexSquare *square;
	int xi, yi, wd, ht, wh, hh;
	int wdecr, hdecr, xh, yh;
    
	wdecr=w; hdecr=h; xh=x; yh=y;

	for(yi = 0; hdecr>0 && yi < texnumy; yi++) {
		if(yi < texnumy - 1) {
			ht = texture_height;
		} else {
			ht = image_height - texture_height * yi;
		}

		xh =x;
		wdecr =w;

		for(xi = 0; wdecr>0 && xi < texnumx; xi++) {
			square = texgrid + yi * texnumx + xi;

			if (xi < texnumx - 1) {
				wd = texture_width;
			} else {
				wd = image_width - texture_width * xi;
			}

			if(0 <= xh && xh < wd && 0 <= yh && yh < ht) {
				square->isDirty=GL_TRUE;

				wh=(wdecr<wd)?wdecr:wd-xh;
				if(wh<0) wh=0;

				hh=(hdecr<ht)?hdecr:ht-yh;
				if(hh<0) hh=0;

				if(xh<square->dirtyXoff)
					square->dirtyXoff=xh;

				if(yh<square->dirtyYoff)
					square->dirtyYoff=yh;

				square->dirtyWidth = wd-square->dirtyXoff;
				square->dirtyHeight = ht-square->dirtyYoff;
		
				wdecr-=wh;

				if(xi == texnumx - 1) {
					hdecr-=hh;
				}
			}

			xh-=wd;
			if(xh<0) xh=0;
		}
		yh-=ht;
		if(yh<0) yh=0;
	}
}
static void draw_textures ()
{
	struct TexSquare *square = texgrid;
	int x, y;
	GLenum err;

	glColor3f(1.0,1.0,1.0);

	for(y = 0; y < texnumy; y++) {
		for(x = 0; x < texnumx; x++) {
			if(square->isTexture==GL_FALSE) {
				continue;
			}

			glBindTexture (GL_TEXTURE_2D, square->texobj);
			err = glGetError ();
			if(err==GL_INVALID_ENUM) {
				mp_msg (MSGT_VO, MSGL_ERR, "GLERROR glBindTexture := GL_INVALID_ENUM, texnum x=%d, y=%d, texture=%d\n", x, y, square->texobj);
			} else if(err==GL_INVALID_OPERATION) {
				mp_msg (MSGT_VO, MSGL_V, "GLERROR glBindTexture := GL_INVALID_OPERATION, texnum x=%d, y=%d, texture=%d\n", x, y, square->texobj);
			}

			if(glIsTexture(square->texobj) == GL_FALSE) {
				square->isTexture=GL_FALSE;
				mp_msg (MSGT_VO, MSGL_ERR, "GLERROR ain't a texture(update): texnum x=%d, y=%d, texture=%d\n",
					x, y, square->texobj);
			}

			if(square->isDirty) {
				if(use_rgba) {
					glTexSubImage2D (GL_TEXTURE_2D, 0, 
						square->dirtyXoff, square->dirtyYoff,
						square->dirtyWidth, square->dirtyHeight,
						GL_RGBA, GL_UNSIGNED_BYTE, square->texture);
				} else if(use_rgb) {
					glTexSubImage2D (GL_TEXTURE_2D, 0, 
						square->dirtyXoff, square->dirtyYoff,
						square->dirtyWidth, square->dirtyHeight,
						GL_RGB, GL_UNSIGNED_BYTE, square->texture);
				} else if(use_abgr) {
					glTexSubImage2D (GL_TEXTURE_2D, 0, 
						square->dirtyXoff, square->dirtyYoff,
						square->dirtyWidth, square->dirtyHeight,
						GL_ABGR_EXT, GL_UNSIGNED_BYTE, square->texture);
				}
				square->isDirty=GL_FALSE;
				square->dirtyXoff=0; square->dirtyYoff=0; square->dirtyWidth=-1; square->dirtyHeight=-1;
			}

			glBegin(GL_QUADS);

			glTexCoord2f (0, 0);
			glVertex2f (square->fx1, square->fy1);

			glTexCoord2f (0, square->ycov);
			glVertex2f (square->fx4, square->fy4);

			glTexCoord2f (square->xcov, square->ycov);
			glVertex2f (square->fx3, square->fy3);

			glTexCoord2f (square->xcov, 0);
			glVertex2f (square->fx2, square->fy2);

			glEnd();
		} /* for all texnumx */
	} /* for all texnumy */

	(void) glGetError ();
}
static int setup_textures(uint32_t d_width, uint32_t d_height)
{
	struct TexSquare *tsq=0;
	int e_x, e_y, s, i=0;
	int x=0, y=0;

	int gl_internal_format;
	int gl_bitmap_format;

	GLint format=0;
	GLenum err;

	if(use_rgb) {
		gl_internal_format = gl_bitmap_format = GL_RGB;
		image_bytes = 3;
	} else if(use_rgba) {
		gl_internal_format = gl_bitmap_format = GL_RGBA;
		image_bytes = 4;
        } else if(use_abgr) {
                gl_internal_format = GL_RGBA;
                gl_bitmap_format = GL_ABGR_EXT;
                image_bytes = 4;
	}
	/*	
	 * Texture size detection doesn't handle GL_ABGR_EXT gracefully,
	 * so i determine the size with GL_RGBA and switch later to
	 * GL_ABGR_EXT
	 */
	imagedata=malloc(image_width*(image_height+1)*image_bytes);
	memset(imagedata,128,image_width*image_height*image_bytes);

	texture_width=image_width;
	texture_height=image_height;

	e_x=0; s=1;
	while (s<texture_width){ s*=2; e_x++; }
	texture_width=s;

	e_y=0; s=1;
	while (s<texture_height){ s*=2; e_y++; }
	texture_height=s;

	/* Test the max texture size */
	do {
		glTexImage2D (GL_PROXY_TEXTURE_2D, 0,
			gl_internal_format,
			texture_width, texture_height,
			0, gl_bitmap_format, GL_UNSIGNED_BYTE, NULL); 

		glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, &format);

		if(format != gl_internal_format) {
			mp_msg (MSGT_VO, MSGL_V, "[sgi] Needed texture [%dx%d] too big, trying ",
				texture_height, texture_width);

			if(texture_width > texture_height) {
				e_x--;
				texture_width = 1;
				for (i = e_x; i > 0; i--)
					texture_width *= 2;
			} else {
				e_y--;
				texture_height = 1;
				for (i = e_y; i > 0; i--)
					texture_height *= 2;
			}

			mp_msg (MSGT_VO, MSGL_V, "[%dx%d] !\n", texture_height, texture_width);

			if(texture_width < 64 || texture_height < 64) {
				mp_msg (MSGT_VO, MSGL_FATAL, "[sgi] Unable to initialize textures !\n");
				return -1;
			}
		}
	} while (format != gl_internal_format && texture_width > 1 && texture_height > 1);

	texnumx = image_width / texture_width;
	if ((image_width % texture_width) > 0)
		texnumx++;

	texnumy = image_height / texture_height;
	if ((image_height % texture_height) > 0)
		texnumy++;

	mp_msg(MSGT_VO, MSGL_V, "[sgi] Creating %dx%d textures of size %dx%d ...\n",
		texnumx, texnumy, texture_width,texture_height);

	/* Allocate the texture memory */

	texpercx = (GLfloat) texture_width / (GLfloat) image_width;
	if (texpercx > 1.0)
		texpercx = 1.0;

	texpercy = (GLfloat) texture_height / (GLfloat) image_height;
	if (texpercy > 1.0)
		texpercy = 1.0;

	texgrid = (struct TexSquare *)calloc(texnumx * texnumy, sizeof (struct TexSquare));

	raw_line_len = image_width * image_bytes;

	mp_msg (MSGT_VO, MSGL_DBG2, "[sgi] texture-usage %d*width=%d, %d*height=%d\n",
		(int) texnumx, (int) texture_width, (int) texnumy,
		(int) texture_height);

	for(y = 0; y < texnumy; y++) {
		for(x = 0; x < texnumx; x++) {
			tsq = texgrid + y * texnumx + x;

			if (x == texnumx - 1 && image_width % texture_width)
				tsq->xcov =
					(GLfloat) (image_width % texture_width) / (GLfloat) texture_width;
			else
				tsq->xcov = 1.0;

			if (y == texnumy - 1 && image_height % texture_height)
				tsq->ycov = 
					(GLfloat) (image_height % texture_height) / (GLfloat) texture_height;
			else
				tsq->ycov = 1.0;

			tsq->fx1 = (float)(x)*texpercx;
			if(tsq->fx1>1.0) tsq->fx1=1.0;
			tsq->fy1 = (float)(y)*texpercy;
			if(tsq->fy1>1.0) tsq->fy1=1.0;

			tsq->fx2 = (float)(x+1)*texpercx;
			if(tsq->fx2>1.0) tsq->fx2=1.0;
			tsq->fy2 = tsq->fy1;

			tsq->fx3 = tsq->fx2;
			tsq->fy3 = (float)(y+1)*texpercy;
			if(tsq->fy3>1.0) tsq->fy3=1.0;

			tsq->fx4 = tsq->fx1;
			tsq->fy4 = tsq->fy3;


			tsq->isDirty=GL_TRUE;
			tsq->isTexture=GL_FALSE;
			tsq->texobj=0;
			tsq->dirtyXoff=0; tsq->dirtyYoff=0; tsq->dirtyWidth=-1; tsq->dirtyHeight=-1;

			glGenTextures (1, &(tsq->texobj));

			glBindTexture (GL_TEXTURE_2D, tsq->texobj);
			err = glGetError ();
			if(err==GL_INVALID_ENUM) {
				mp_msg (MSGT_VO, MSGL_ERR, "GLERROR glBindTexture (glGenText) := GL_INVALID_ENUM, texnum x=%d, y=%d, texture=%d\n", x, y, tsq->texobj);
			} 

			if(glIsTexture(tsq->texobj) == GL_FALSE) {
				mp_msg (MSGT_VO, MSGL_ERR, "GLERROR ain't a texture (glGenText): texnum x=%d, y=%d, texture=%d\n",
					x, y, tsq->texobj);
			} else {
				tsq->isTexture=GL_TRUE;
			}

			glTexImage2D (GL_TEXTURE_2D, 0,
				gl_internal_format,
				texture_width, texture_height,
				0, gl_bitmap_format, GL_UNSIGNED_BYTE, NULL); 

			glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_PRIORITY, 1.0);

			glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
			glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

			glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
			glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

			glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

		}	/* for all texnumx */
	}  /* for all texnumy */
	resetTexturePointers (imagedata);

	glEnable(GL_TEXTURE_2D);

	resize((int *)&d_width, (int *)&d_height);

	glClearColor( 0.0f,0.0f,0.0f,0.0f );
	glClear( GL_COLOR_BUFFER_BIT );

	draw_textures();

	free(imagedata);
	imagedata = NULL;

	return 0;
}

static int inv_table[] = {104597, 132201, 25675, 53279};
static void * yuvTable;	// pointer to the yuv->rgb table start so it can be freed()
static void * table_rV[256];
static void * table_gU[256];
static int    table_gV[256];
static void * table_bU[256];

static int div_round (int dividend, int divisor)
{
    if (dividend > 0) return (dividend + (divisor>>1)) / divisor;
    else return -((-dividend + (divisor>>1)) / divisor);
}
static void setup_softcs(void)
{  
	int brightness = 0;
	int contrast = 1<<16;
	int saturation = 1<<16;

	int i;
	uint8_t table_Y[1024];
	uint8_t *table_8 = 0;
	int entry_size = 0;
	void *table_r = 0, *table_g = 0, *table_b = 0;
	void *table_start;

	int64_t crv =  inv_table[0];
	int64_t cbu =  inv_table[1];
	int64_t cgu = -inv_table[2];
	int64_t cgv = -inv_table[3];
	int64_t cy  = 1<<16;
	int64_t oy  = 0;

	cy= (cy*255) / 219;
	oy= 16<<16;
	
	cy = (cy *contrast             )>>16;
	crv= (crv*contrast * saturation)>>32;
	cbu= (cbu*contrast * saturation)>>32;
	cgu= (cgu*contrast * saturation)>>32;
	cgv= (cgv*contrast * saturation)>>32;
	oy -= 256*brightness;

	for(i = 0; i < 1024; i++) {
		int j;

		j= (cy*(((i - 384)<<16) - oy) + (1<<31))>>32;
		j = (j < 0) ? 0 : ((j > 255) ? 255 : j);
		table_Y[i] = j;
	}

	table_start= table_8 = malloc ((256 + 2*232) * sizeof (uint8_t));

	entry_size = sizeof (uint8_t);
	table_r = table_g = table_b = table_8 + 232;

	for(i = -232; i < 256+232; i++) {
		((uint8_t * )table_b)[i] = table_Y[i+384];
	}

	for(i = 0; i < 256; i++) {
		table_rV[i] = (char*)table_r + entry_size * div_round (crv * (i-128), 76309);
		table_gU[i] = (char*)table_g + entry_size * div_round (cgu * (i-128), 76309);
		table_gV[i] = entry_size * div_round (cgv * (i-128), 76309);
		table_bU[i] = (char*)table_b + entry_size * div_round (cbu * (i-128), 76309);
	}

	if(yuvTable) free(yuvTable);
	yuvTable= table_start;
}

static void setup_colormatrix(int format)
{
		GLfloat yuv2rgb[16] = {1.000,  1.000,  1.000,  0.000, 
                                       0.000, -0.344,  1.770,  0.000, 
                                       1.403, -0.714,  0.000,  0.000, 
                                       0.000,  0.000,  0.000,  1.000}; 

		if(use_abgr) {
			for(int i = 0; i < 4; i++) {
				GLfloat tmp;
				tmp=yuv2rgb[i];
				yuv2rgb[i]=yuv2rgb[12+i];
				yuv2rgb[12+i]=tmp;
				tmp=yuv2rgb[4+i];
				yuv2rgb[4+i]=yuv2rgb[8+i];
				yuv2rgb[8+i]=tmp;
			}
		}
		if(format == IMGFMT_YV12) {
			glDisable(GL_ALPHA_TEST);
			glDisable(GL_DEPTH_TEST);
			glDisable(GL_BLEND);
			glDisable(GL_DITHER);
			glDisable(GL_FOG);
			glDisable(GL_LOGIC_OP);
			glDisable(GL_STENCIL_TEST);

			glPixelTransferi(GL_MAP_COLOR, GL_FALSE); 
			glPixelTransferi(GL_RED_SCALE, 1); 
			glPixelTransferi(GL_RED_BIAS, 0); 
			glPixelTransferi(GL_GREEN_SCALE, 1); 
			glPixelTransferi(GL_GREEN_BIAS, 0); 
			glPixelTransferi(GL_BLUE_SCALE, 1); 
			glPixelTransferi(GL_BLUE_BIAS, 0); 
			glPixelTransferi(GL_ALPHA_SCALE, 1); 
			glPixelTransferi(GL_ALPHA_BIAS, 0); 
			glMatrixMode(GL_COLOR); 
			glLoadMatrixf(yuv2rgb); 
			glPixelTransferf(GL_GREEN_BIAS, -0.5); 
			glPixelTransferf(GL_BLUE_BIAS, -0.5); 
			glMatrixMode(GL_MODELVIEW);
		}
}

static void setup_pixeltex(void)
{
		float ptg[16][16][16][4];
		for(int z = 0; z < 16; z++) {
			for(int y = 0; y < 16; y++) {
				for(int x = 0; x < 16; x++) {
					float tx = ((float)x)/15.0f; if(tx > 0.9375f) tx = 0.9375f;
					float ty = ((float)y)/15.0f;
					float tz = ((float)z)/15.0f;
					ptg[z][y][x][0] = tx+((tz-0.5f)*1.403f);
					if(ptg[z][y][x][0] < 0.0f) ptg[z][y][x][0] = 0.0f;
					else if(ptg[z][y][x][0] > 1.0f) ptg[z][y][x][0] = 1.0f;

					ptg[z][y][x][1] = tx-((ty-0.5f)*0.344f)-((tz-0.5f)*0.714f);
					if(ptg[z][y][x][1] < 0.0f) ptg[z][y][x][1] = 0.0f;
					else if(ptg[z][y][x][1] > 1.0f) ptg[z][y][x][1] = 1.0f;

					ptg[z][y][x][2] = tx+((ty-0.5f)*1.770f);
					if(ptg[z][y][x][2] < 0.0f) ptg[z][y][x][2] = 0.0f;
					else if(ptg[z][y][x][2] > 1.0f) ptg[z][y][x][2] = 1.0f;

					ptg[z][y][x][3] = 1.0f;
				}
			}
		}
		glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_RGBA, 16, 16, 16, 0, GL_RGBA, GL_FLOAT, (const float *)ptg);
		glTexParameteri (GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTexParameteri (GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glEnable(GL_TEXTURE_3D_EXT);

		if(hw_detected == HW_MGRAS) {
			glEnable(GL_PIXEL_TEX_GEN_SGIX);
		} else if(hw_detected == HW_VPRO) {
			glEnable(GL_PIXEL_TEXTURE_SGIS);
		}
}

static int config_glx(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format)
{
	if(WinID >= 0) {
		vo_window = WinID ? (Window)WinID : mRootWin;
		vo_x11_selectinput_witherr(mDisplay, vo_window,
             StructureNotifyMask | KeyPressMask | PointerMotionMask |
             ButtonPressMask | ButtonReleaseMask | ExposureMask);
		return 0;
	}
	if(vo_window == None) {
		XSizeHints hint;
		XVisualInfo *vinfo, vinfo_buf;
		Colormap cmap;
		XEvent xev;

		hint.x = vo_dx;
		hint.y = vo_dy;
		hint.width = d_width;
		hint.height = d_height;
		hint.flags = PPosition | PSize;

		/* Make the window */
		int attrListDouble[] = {
			GLX_RGBA,
			GLX_DOUBLEBUFFER,
			GLX_RED_SIZE, 8,
			GLX_BLUE_SIZE, 8,
			GLX_GREEN_SIZE, 8,
			GLX_DEPTH_SIZE, 0,
			GLX_STENCIL_SIZE, 0,
			GLX_ALPHA_SIZE, 0,
			None
		};
		int backupAttrListDouble[] = {
			GLX_RGBA,
			GLX_DOUBLEBUFFER,
			GLX_RED_SIZE, 1,
			GLX_BLUE_SIZE, 1,
			GLX_GREEN_SIZE, 1,
			GLX_DEPTH_SIZE, 0,
			GLX_STENCIL_SIZE, 0,
			GLX_ALPHA_SIZE, 0,
			None
		};		
		int attrListSingle[] = {
			GLX_RGBA,
			GLX_RED_SIZE, 8,
			GLX_BLUE_SIZE, 8,
			GLX_GREEN_SIZE, 8,
			GLX_DEPTH_SIZE, 0,
			GLX_STENCIL_SIZE, 0,
			GLX_ALPHA_SIZE, 0,
			None
		};
		int backupAttrListSingle[] = {
			GLX_RGBA,
			GLX_RED_SIZE, 1,
			GLX_BLUE_SIZE, 1,
			GLX_GREEN_SIZE, 1,
			GLX_DEPTH_SIZE, 0,
			GLX_STENCIL_SIZE, 0,
			GLX_ALPHA_SIZE, 0,
			None
		};		

		if(double_buffer) {
			vinfo = glXChooseVisual(mDisplay, mScreen, attrListDouble);
			if(vinfo == NULL) {
				vinfo = glXChooseVisual(mDisplay, mScreen, backupAttrListDouble);
			}
		} else {
			vinfo = glXChooseVisual(mDisplay, mScreen, attrListSingle);
			if(vinfo == NULL) {
				vinfo = glXChooseVisual(mDisplay, mScreen, backupAttrListSingle);
			}
		}
		if(vinfo == NULL) {
			mp_msg(MSGT_VO, MSGL_FATAL, "[sgi] no GLX support present\n");
			return -1;
		}
		{
			char vomsgbuf[128];
			sprintf(vomsgbuf, "[sgi] Chose visual 0x%X with depth %d\n", (int)vinfo->visualid, vinfo->depth);
			mp_msg(MSGT_VO, MSGL_INFO, vomsgbuf);
		}

		vo_fs = VO_FALSE;
		cmap = vo_x11_create_colormap(vinfo);
		vo_window = vo_x11_create_smooth_window(mDisplay, RootWindow(mDisplay,mScreen), 
			vinfo->visual, hint.x, hint.y, hint.width, hint.height, vinfo->depth, cmap);

		XSelectInput(mDisplay, vo_window, StructureNotifyMask);

		/* Tell other applications about this window */
		XSetStandardProperties(mDisplay, vo_window, title, title, None, NULL, 0, &hint);

		/* Map window. */
		XMapWindow(mDisplay, vo_window);
		vo_x11_sizehint( hint.x, hint.y, hint.width, hint.height,0 );
		XClearWindow(mDisplay,vo_window);

		/* Wait for map. */
		do {
			XNextEvent(mDisplay, &xev);
		} while (xev.type != MapNotify || xev.xmap.event != vo_window);

		vo_x11_classhint( mDisplay,vo_window,"sgi" );
		//vo_hidecursor(mDisplay,vo_window);
  
		XSync(mDisplay, False);

        vo_x11_selectinput_witherr(mDisplay, vo_window, StructureNotifyMask | KeyPressMask | 
			PointerMotionMask | ButtonPressMask | ButtonReleaseMask | ExposureMask);
	}
	vo_x11_nofs_sizepos(vo_dx, vo_dy, d_width, d_height);
	if(vo_fs ^ (flags & VOFLAG_FULLSCREEN)) {
		vo_x11_fullscreen();
	}

	return 0;
}

static void check_dcd(void)
{
	inventory_t *invp;
	setinvent();
	while (invp = getinvent()) { 
		if (invp->inv_class != INV_DISPLAY)
		continue;
		if (invp->inv_type == INV_DCD_BOARD) {
			has_dcd = 1;
		}
	}
}

static void config_render_path(uint32_t format)
{
	char *renderer, *extension;

	use_ycrcb = use_colormatrix = use_drawpixels = use_textures = 0;
	use_rgb = use_rgba = use_abgr = use_async = use_rendertotex = 0;
	use_pixeltex = use_softcs = 0;

	hw_detected = HW_UNKNOWN;

	renderer = (char *)glGetString(GL_RENDERER);

	if(strstr(renderer, "CRIME")){
		hw_detected = HW_O2;
		if (!has_dcd) {
			mp_msg(MSGT_VO, MSGL_INFO, "[sgi] O2 (CRM) Hardware detected:\n");
		} else {
			mp_msg(MSGT_VO, MSGL_INFO, "[sgi] O2 (CRM) Hardware with Dual Channel Display detected:\n");
		}

		// Setup defaults
		if(format == IMGFMT_UYVY || format == IMGFMT_YV12){
			use_ycrcb = 1;
			use_textures = 1;
			use_rgba = 1;
		} else if(format == IMGFMT_RGB24){
			use_drawpixels = 1;
			use_rgb = 1;
		}

		if(force_rgba){
			use_rgba = 1;
			use_rgb = 0;
		} else if(force_rgb){
			use_rgb = 1;
			use_rgba = 0;
		}

		if(force_colormatrix){
			use_colormatrix = 1;
			use_ycrcb = 0;
			use_softcs = 0;
		} else if(force_softcs){
			use_softcs = 1;
			use_colormatrix = 0;
			use_ycrcb = 0;
			use_rgba = 0;
			use_rgb = 1;
		}

		if(force_drawpixels){
			use_drawpixels = 1;
			use_textures = 0;
		} else if(force_textures){
			use_textures = 1;
			use_drawpixels = 0;
		}

		if(use_textures && use_ycrcb){
			use_rgba = 1;
			use_rgb = 0;
		}

		if(force_abgr){
			use_abgr = 1;
			use_rgb = use_rgba = 0;
		}
	} else if(strstr(renderer, "IMPACT") && strstr(renderer, "/4")){
		mp_msg(MSGT_VO, MSGL_INFO, "[sgi] Impact (MGRAS) Hardware with 4MB TRAM detected:\n");
		hw_detected = HW_MGRAS;
		
		// Setup defaults
		use_textures = 1;
		use_rgb = 1;
		if(format == IMGFMT_YV12)
			use_softcs = 1;

		if(force_drawpixels){
			use_textures = 0;
			use_drawpixels = 1;
		}

		if(force_rgba){
			use_rgba = 1;
			use_rgb = 0;
		}

		if(force_pixeltex){
			use_pixeltex = 1;
			use_softcs = 0;
			use_colormatrix = 0;
			use_textures = 0;
			use_drawpixels = 1;
		} else if(force_colormatrix){
			use_pixeltex = 0;
			use_colormatrix = 1;
			use_softcs = 0;
		}

		if(force_abgr){
			use_abgr = 1;
			use_rgb = use_rgba = 0;
		}
	} else if(strstr(renderer, "IMPACT") && 
	  (strstr(renderer, "/1/0") || strstr(renderer, "/2/0") || 
	   strstr(renderer, "/1/1") || strstr(renderer, "/2/1"))){
		mp_msg(MSGT_VO, MSGL_INFO, "[sgi] Impact (MGRAS) Hardware with 0 or 1 MB TRAM detected:\n");
		hw_detected = HW_MGRAS;

		// Setup defaults
		use_drawpixels = 1;
		use_rgb = 1;
		if(format == IMGFMT_YV12)
			use_softcs = 1;

		if(force_textures){
			use_textures = 1;
			use_drawpixels = 0;
		}

		if(force_rgba){
			use_rgba = 1;
			use_rgb = 0;
		}

		if(force_pixeltex){
			use_pixeltex = 1;
			use_softcs = 0;
			use_colormatrix = 0;
			use_textures = 0;
			use_drawpixels = 1;
		} else if(force_colormatrix){
			use_pixeltex = 0;
			use_colormatrix = 1;
			use_softcs = 0;
		}

		if(force_abgr){
			use_abgr = 1;
			use_rgb = use_rgba = 0;
		}
	} else if(strstr(renderer, "VPRO")){
		hw_detected = HW_VPRO;
		if (!has_dcd) {
			mp_msg(MSGT_VO, MSGL_INFO, "[sgi] VPro (Odyssey) Hardware detected:\n");
		} else {
			mp_msg(MSGT_VO, MSGL_INFO, "[sgi] VPro (Odyssey) Hardware with Dual Channel Display detected:\n");
		}

		// Setup defaults
		use_textures = 1;
		use_rgb = 1;
		if(format == IMGFMT_YV12)
			use_softcs = 1;

		if(force_rgba){
			use_rgba = 1;
			use_rgb = 0;
		} 

		if(force_pixeltex){
			use_pixeltex = 1;
			use_colormatrix = 0;
			use_softcs = 0;
			use_textures = 0;
			use_drawpixels = 1;
			if(force_rendertotex){
				use_rendertotex = 1;
			}
		} else if(force_colormatrix){
			use_colormatrix = 1;
			use_pixeltex = 0;
			use_softcs = 0;
		}

		if(force_drawpixels){
			use_textures = 0;
			use_drawpixels = 1;

			if(force_async){
				use_async = 1;

				async_marker = glGenAsyncMarkersSGIX(1);
				glAsyncMarkerSGIX(async_marker);
				async_first = 0;
				glEnable(GL_ASYNC_DRAW_PIXELS_SGIX);
			}
		}

		if(force_abgr){
			use_abgr = 1;
			use_rgb = use_rgba = 0;
		}

	} else if(strstr(renderer, "IR")){
		mp_msg(MSGT_VO, MSGL_INFO, "[sgi] Infinite Reality (KONA) Hardware detected:\n");
		hw_detected = HW_IR;

		// Setup defaults
		use_textures = 1;
		use_rgb = 1;
		if(format == IMGFMT_YV12)
			use_softcs = 1;

		if(force_rgba){
			use_rgba = 1;
			use_rgb = 0;
		}

		if(force_colormatrix){
			use_colormatrix = 1;
			use_softcs = 0;
		}

		if(force_drawpixels){
			use_textures = 0;
			use_drawpixels = 1;
		}

		if(force_abgr){
			use_abgr = 1;
			use_rgb = use_rgba = 0;
		}

	} else if(strstr(renderer, "Fire")){
		mp_msg(MSGT_VO, MSGL_INFO, "[sgi] Ultimate Vision (Voyager) Hardware detected:\n");
		hw_detected = HW_UV;

		// Setup defaults
		use_textures = 1;
		use_rgb = 1;
		if(format == IMGFMT_YV12)
			use_softcs = 1;

		if(force_rgba){
			use_rgba = 1;
			use_rgb = 0;
		}

		if(force_pixeltex){
			use_pixeltex = 1;
			use_colormatrix = 0;
			use_softcs = 0;
			use_textures = 0;
			use_drawpixels = 1;
			if(force_rendertotex){
				use_drawpixels = 0;
				use_textures = 1;
				use_rendertotex = 1;
			}
		} else if(force_colormatrix){
			use_colormatrix = 1;
			use_pixeltex = 0;
			use_softcs = 0;
		}

		if(force_drawpixels){
			use_textures = 0;
			use_drawpixels = 1;
		}

		if(force_abgr){
			use_abgr = 1;
			use_rgb = use_rgba = 0;
		}

	} else if(strstr(renderer, "NEWPORT") || strstr(renderer, "LIGHT")) {
		mp_msg(MSGT_VO, MSGL_INFO, "[sgi] Newport (Entry) Hardware detected:\n");
		hw_detected = HW_ENTRY;

		// Setup defaults
		double_buffer = 0;
		use_drawpixels = 1;
		if(format == IMGFMT_YV12)
			use_softcs = 1;

		extension = (char *)glGetString(GL_EXTENSIONS);

   		if (strstr(extension, "EXT_abgr")) {
			mp_msg(MSGT_VO, MSGL_INFO, "ABGR pixel order selected\n");
			use_abgr = 1;
      	} else {
			use_rgb = 1;
		}

		if(force_textures) {
			use_textures = 1; 
			use_drawpixels = 0;
		}

		if(force_colormatrix) {
			use_colormatrix = 1;
			use_softcs = 0;
		}

		if(force_rgba) {
			use_abgr = 0;
			use_rgb = 0;
			use_rgba = 1;
		}
		if(force_rgb) {
			use_abgr = 0;
			use_rgb = 1;
			use_rgba = 0;
		}

	} else if(strstr(renderer, "-XS") || strstr(renderer, "-XZ") || strstr(renderer, "-Elan") || strstr(renderer, "-Extreme")) {
		mp_msg(MSGT_VO, MSGL_INFO, "[sgi] GR2/GR3/GU1 (Express) Hardware detected:\n");
		hw_detected = HW_EXPRESS;

		// Setup defaults
		double_buffer = 0;
		use_drawpixels = 1;
		if(format == IMGFMT_YV12)
			use_softcs = 1;

		extension = (char *)glGetString(GL_EXTENSIONS);

   		if (strstr(extension, "EXT_abgr")){
			mp_msg(MSGT_VO, MSGL_INFO, "ABGR pixel order selected\n");
			use_abgr = 1;
		} else {
			use_rgb = 1;
		}

		if(force_textures) {
			use_textures = 1; 
			use_drawpixels = 0;
		}

		if(force_colormatrix) {
			use_colormatrix = 1; 
			use_softcs = 0;
		}

		if(force_rgba){
			use_abgr = 0; 
			use_rgb = 0; 
			use_rgba = 1;
		}

		if(force_rgb){
			use_abgr = 0;
			use_rgb = 1;
			use_rgba = 0;
		}

	} else {
		mp_msg(MSGT_VO, MSGL_INFO, "[sgi] Unknown Hardware detected:\n");

		if(force_ycrcb) use_ycrcb = 1;
		else if(force_colormatrix) use_colormatrix = 1;
		else use_softcs = 1;

		if(force_textures) use_textures = 1;
		else use_drawpixels = 1;

		if(force_rgba) use_rgba = 1;
		else use_rgb = 1;
	}

	if(format == IMGFMT_RGB24){
		use_rgb = 1;
		use_rgba = 0;
	}

	if(use_ycrcb) mp_msg(MSGT_VO, MSGL_INFO, "\tusing YCrCb 4:2:2,\n");
	else if(use_colormatrix){
		mp_msg(MSGT_VO, MSGL_INFO, "\tusing color_matrix ");
		if(use_rgb) mp_msg(MSGT_VO, MSGL_INFO, "(rgb)\n");
		else if(use_rgba) mp_msg(MSGT_VO, MSGL_INFO, "(rgba)\n");
		else if(use_abgr) mp_msg(MSGT_VO, MSGL_INFO, "(abgr)\n");
	}
	else if(use_pixeltex){
		mp_msg(MSGT_VO, MSGL_INFO, "\tusing pixel_texture ");
		if(use_rgb) mp_msg(MSGT_VO, MSGL_INFO, "(rgb)\n");
		else if(use_rgba) mp_msg(MSGT_VO, MSGL_INFO, "(rgba)\n");
		else if(use_abgr) mp_msg(MSGT_VO, MSGL_INFO, "(abgr)\n");
	}
	else if (use_softcs){
		mp_msg(MSGT_VO, MSGL_INFO, "\tusing software colorspace conversion ");
		if(use_rgb) mp_msg(MSGT_VO, MSGL_INFO, "(rgb)\n");
		else if(use_rgba) mp_msg(MSGT_VO, MSGL_INFO, "(rgba)\n");
		else if(use_abgr) mp_msg(MSGT_VO, MSGL_INFO, "(abgr)\n");
	}
	if(use_drawpixels) mp_msg(MSGT_VO, MSGL_INFO, "\tdrawing via glDrawPixels\n");
	if(use_textures) mp_msg(MSGT_VO, MSGL_INFO, "\tdrawing via textures\n");
	if(use_async) mp_msg(MSGT_VO, MSGL_INFO, "\tasync\n");
}

#ifdef HAVE_NEW_GUI
static int config_glx_gui(uint32_t d_width, uint32_t d_height) {
  vo_dwidth = d_width;
  vo_dheight = d_height;
  guiGetEvent( guiSetShVideo,0 ); // the GUI will set up / resize the window
  return 0;
}
#endif

static XVisualInfo *getWindowVisualInfo(Window win) {
  XWindowAttributes xw_attr;
  XVisualInfo vinfo_template;
  int tmp;
  XGetWindowAttributes(mDisplay, win, &xw_attr);
  vinfo_template.visualid = XVisualIDFromVisual(xw_attr.visual);
  return XGetVisualInfo(mDisplay, VisualIDMask, &vinfo_template, &tmp);
}

static int setGlWindow(XVisualInfo **vinfo, GLXContext *context, Window win)
{
	XVisualInfo *new_vinfo;
	GLXContext new_context = NULL;
	int keep_context = 0;

	// should only be needed when keeping context, but not doing glFinish
	// can cause flickering even when we do not keep it.
	if(*context) {
		glFinish();
	}

	new_vinfo = getWindowVisualInfo(win);
	if(*context && *vinfo && new_vinfo && 
			(*vinfo)->visualid == new_vinfo->visualid) {
		// we can keep the GLXContext
		new_context = *context;
		XFree(new_vinfo);
		new_vinfo = *vinfo;
		keep_context = 1;
	} else {
		// create a context
		new_context = glXCreateContext(mDisplay, new_vinfo, NULL, True);
		if(!new_context) {
			mp_msg(MSGT_VO, MSGL_FATAL, "[gl] Could not create GLX context!\n");
			XFree(new_vinfo);
			return 1;
		}
	}

	// set context
	if(!glXMakeCurrent(mDisplay, vo_window, new_context)) {
		mp_msg (MSGT_VO, MSGL_FATAL, "[gl] Could not set GLX context!\n");
		if(!keep_context) {
			glXDestroyContext (mDisplay, new_context);
			XFree(new_vinfo);
		}
		return 1;
	}

	// set new values
	vo_window = win;
	{
		Window root;
		int tmp;
		unsigned utmp;
		XGetGeometry(mDisplay, vo_window, &root, &tmp, &tmp,
			(unsigned *)&vo_dwidth, (unsigned *)&vo_dheight, &utmp, &utmp);
	}
	if(!keep_context) {
		void *(*getProcAddress)(const GLubyte *);
		const char *(*glXExtStr)(Display *, int);

		if(*context) {
			glXDestroyContext(mDisplay, *context);
		}
		*context = new_context;

		// and inform that reinit is neccessary
		return 1;
	}
	return 0;
}

static int config(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format)
{
	image_height = height;
	image_width = ((width+7)&0xfffffff8);
	image_format = format;
	paused = 0;
	check_dcd();
	if(has_dcd) {
		if(vo_screenwidth >= vo_screenheight/3*8) {
			vo_screenwidth = vo_screenwidth/2;
		}
	}
	
	panscan_init();
	aspect_save_orig(width,height);
	aspect_save_prescale(d_width,d_height);
	aspect_save_screenres(vo_screenwidth, vo_screenheight);

	aspect(&d_width,&d_height,A_NOZOOM);
	vo_dx = (int)(vo_screenwidth - d_width)/2;
	vo_dy = (int)(vo_screenheight - d_height)/2;
	vo_dwidth = d_width;
	vo_dheight = d_height;
	geometry(&vo_dx, &vo_dy, &d_width, &d_height,
	          vo_screenwidth, vo_screenheight);

	#if defined(HAVE_NEW_GUI)
	if(use_gui) {
	  if (config_glx_gui(d_width, d_height) == -1)
	    return -1;
	} else
	#endif
	if (config_glx(width, height, d_width, d_height, flags, title, format) == -1)
		return -1;

	setGlWindow(&gl_vinfo, &gl_context, vo_window);

	config_render_path(format);

	glDisable(GL_BLEND); 
	glDisable(GL_DEPTH_TEST);
	glDepthMask(GL_FALSE);
	glDisable(GL_CULL_FACE);

	if(use_colormatrix) {
		setup_colormatrix(format);
	} else if(use_pixeltex) {
		if(!use_rendertotex) setup_pixeltex();
	} else if(use_softcs) {
		setup_softcs();
	}

	if(use_textures) {
		glEnable(GL_TEXTURE_2D);
		setup_textures(d_width, d_height);

		if(use_ycrcb && use_rgba) {	//O2 magic
			int configcount;
			int fbAttrs[] = {GLX_DRAWABLE_TYPE, GLX_PBUFFER_BIT,
				GLX_DOUBLEBUFFER, False, GLX_RED_SIZE, 8,
				GLX_GREEN_SIZE, 8, GLX_BLUE_SIZE, 8, GLX_ALPHA_SIZE, 8, None};
			int pbAttrs[] = {GLX_DIGITAL_MEDIA_PBUFFER_SGIX, GL_TRUE, 
				GLX_PRESERVED_CONTENTS_SGIX, GL_TRUE,
				None};

			DMparams *poolParams;

			/* Set up image/OpenGL attributes for the buffer pool      */
			dmParamsCreate(&_dmParams);
			dmSetImageDefaults(_dmParams, texture_width, texture_height, DM_IMAGE_PACKING_RGBA);
			dmParamsSetEnum(_dmParams,DM_IMAGE_ORIENTATION,DM_IMAGE_TOP_TO_BOTTOM);
			dmParamsSetEnum(_dmParams, DM_IMAGE_LAYOUT, DM_IMAGE_LAYOUT_GRAPHICS);

			dmParamsCreate(&poolParams);
			dmBufferSetPoolDefaults(poolParams, 1, texture_width*texture_height*4, DM_FALSE, DM_TRUE);

			/* Now load poolParams with specific image (GL) information from dmParams. */
			dmBufferGetGLPoolParams(_dmParams, poolParams);

			dmParamsSetEnum(poolParams, DM_POOL_SHARE, DM_POOL_INTERPROCESS);

			/* Create the buffer pool based on all the info. */
			dmBufferCreatePool(poolParams, &_dmpool);
			dmParamsDestroy(poolParams);

			fbConfigs = glXChooseFBConfig(mDisplay, mScreen, fbAttrs, &configcount);
			_dmpbuffer = glXCreateGLXPbufferSGIX(mDisplay, fbConfigs[0], texture_width, texture_height, pbAttrs);
			_pbcontext = glXCreateContextWithConfigSGIX(mDisplay, fbConfigs[0], GLX_RGBA_TYPE_SGIX, NULL, True);

			dmBufferAllocate(_dmpool, &_gfxDMBuffer);
			glXAssociateDMPbufferSGIX(mDisplay, _dmpbuffer, _dmParams, _gfxDMBuffer);
			glXMakeCurrent(mDisplay, _dmpbuffer, _pbcontext);

			/* init viewport */
			glViewport(0,0,texture_width, texture_height);
			/* set up transformation matrix */
			glMatrixMode(GL_MODELVIEW);
			glLoadIdentity();
			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();
			glOrtho (0.0, (double)texture_width, 0.0, (double)texture_height, -1.0, 1.0);

			/* set clear color */
			glClearColor(0.0,0.0,0.0,1.0);
			glDisable(GL_DITHER);
			glDisable(GL_TEXTURE_2D);
			glRasterPos2i (0, 0);

			glXMakeCurrent( mDisplay,vo_window,gl_context );
		}
	}
	glColor3f(1.0f,1.0f,1.0f);

	if(use_pixeltex && use_rendertotex) {
		int configcount;
		int fbAttrs[] = {GLX_DRAWABLE_TYPE, GLX_PBUFFER_BIT,
			GLX_DOUBLEBUFFER, False, GLX_RED_SIZE, 8,
			GLX_GREEN_SIZE, 8, GLX_BLUE_SIZE, 8, GLX_ALPHA_SIZE, 8, GLX_DEPTH_SIZE, 0, None};
		int pbAttrs[] = {GLX_PRESERVED_CONTENTS_SGIX, GL_TRUE,
			None};

		fbConfigs = glXChooseFBConfig(mDisplay, mScreen, fbAttrs, &configcount);
		_dmpbuffer = glXCreateGLXPbufferSGIX(mDisplay, fbConfigs[0], texture_width, texture_height, pbAttrs);
		_pbcontext = glXCreateContextWithConfigSGIX(mDisplay, fbConfigs[0], GLX_RGBA_TYPE_SGIX, NULL, True);

		glXMakeCurrent(mDisplay, _dmpbuffer, _pbcontext);

		/* init viewport */
		glViewport(0,0,texture_width, texture_height);
		/* set up transformation matrix */
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glOrtho (0.0, (double)texture_width, 0.0, (double)texture_height, -1.0, 1.0);

		/* set clear color */
		glClearColor(0.0,0.0,0.0,1.0);
		glDisable(GL_DITHER);
		glDisable(GL_TEXTURE_2D);
		glRasterPos2i (0, 0);
		setup_pixeltex();
		glXMakeCurrent( mDisplay,vo_window,gl_context );
	}
	imagedata = 0;
	if(format == IMGFMT_YV12) {
		if(use_ycrcb) imagedata = (uint8_t *)malloc(image_width*(image_height+1)*2);
		else if(use_rgb) imagedata = (uint8_t *)malloc(image_width*(image_height+1)*3);
		else imagedata = (uint8_t *)malloc(image_width*(image_height+1)*4);

		if(use_textures) resetTexturePointers(imagedata);
	} else {
		imagedata = 0;
	}

	resize((int *)&d_width,(int *)&d_height);

	glClearColor( 0.0f,0.0f,0.0f,0.0f );
	glClear( GL_COLOR_BUFFER_BIT );

	saver_off(mDisplay);  // turning off screen saver

	if (vo_ontop) vo_x11_setlayer(mDisplay, vo_window, vo_ontop);

	return 0;
}

static void gl_set_antialias (int val)
{
  gl_antialias=val;

  if(gl_antialias) {
    glShadeModel (GL_SMOOTH);
    glEnable (GL_POLYGON_SMOOTH);
    glEnable (GL_LINE_SMOOTH);
    glEnable (GL_POINT_SMOOTH);
    mp_msg(MSGT_VO, MSGL_INFO, "[sgi] antialiasing on\n");
  } else {
    glShadeModel (GL_FLAT);
    glDisable (GL_POLYGON_SMOOTH);
    glDisable (GL_LINE_SMOOTH);
    glDisable (GL_POINT_SMOOTH);
    mp_msg(MSGT_VO, MSGL_INFO, "[sgi] antialiasing off\n");
  }
}

static int gl_handlekey(int key)
{
	if(key=='a'||key=='A') {
		gl_set_antialias(!gl_antialias);
		return 0;
	}
	return 1;
}

static void check_events(void)
{
	XEvent         Event;
	char           buf[100];
	KeySym         keySym;
	int            key;
	static XComposeStatus stat;
         
	while( XPending( mDisplay ) ) {
		XNextEvent( mDisplay,&Event );
		if( Event.type == KeyPress ) {
			XLookupString( &Event.xkey,buf,sizeof(buf),&keySym,&stat );
			key = (keySym&0xff00) != 0? ( (keySym&0x00ff) + 256 ) 
		                                 : ( keySym ) ;
			if(gl_handlekey(key)) {
				XPutBackEvent(mDisplay, &Event);
			}
			break;
		} else {
			XPutBackEvent(mDisplay, &Event);
			break;
		}
	}
	int e=vo_x11_check_events(mDisplay);
	if(e&VO_EVENT_RESIZE) resize((int *)&vo_dwidth,(int *)&vo_dheight);
	if(e&VO_EVENT_EXPOSE && paused) flip_page();
}

static void draw_text(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride)
{
	if(image_format == IMGFMT_RGB24) {
		vo_draw_alpha_rgb24(w, h, src, srca, stride,
			imagedata+3*(y0*image_width+x0), 3*image_width);
	} else if(image_format == IMGFMT_YV12) {
		if(use_ycrcb) {
			vo_draw_alpha_uyvy(w, h, src, srca, stride, imagedata+2*(y0*image_width+x0),2*image_width);
		} else if(use_softcs) {
			vo_draw_alpha_rgb24(w, h, src, srca, stride, imagedata+3*(y0*image_width+x0), 3*image_width);
		} else if(use_colormatrix || use_pixeltex) {
			if(use_rgb) {
				int y;
				unsigned char *dstbase = imagedata+3*(y0*image_width+x0);

				for(y = 0; y < h; y++) {
					register unsigned char *dst = dstbase;
					register int x;
					for(x = 0; x < w; x++) {
						if(srca[x]) {
							dst[0] = ((dst[0]*srca[x])>>8)+src[x];
						}
						dst += 3;
					}
					src += stride;
					srca += stride;
					dstbase += (image_width*3);
				}
			} else if(use_rgba) {
				int y;
				unsigned char *dstbase = imagedata+4*(y0*image_width+x0);

				for(y = 0; y < h; y++) {
					register unsigned char *dst = dstbase;
					register int x;
					for(x = 0; x < w; x++){
						if(srca[x]){
							dst[0] = ((dst[0]*srca[x])>>8)+src[x];
						}
						dst += 4;
					}
					src += stride;
					srca += stride;
					dstbase += (image_width*4);
				}
			} else if(use_abgr) {
				int y;
				unsigned char *dstbase = imagedata+4*(y0*image_width+x0);

				for(y = 0; y < h; y++) {
					register unsigned char *dst = dstbase;
					register int x;
					for(x = 0; x < w; x++){
						if(srca[x]){
							dst[3] = ((dst[0]*srca[x])>>8)+src[x];
						}
						dst += 4;
					}
					src += stride;
					srca += stride;
					dstbase += (image_width*4);
				}
			}
		}
	}
}

static void draw_osd(void)
{
	if(!imagedata) return;

	vo_draw_text(image_width, image_height, draw_text);
}

static void flip_page(void)
{
	if(!imagedata) return;

	if (skip_frame)
	{
		frame_skip_counter++;
		if(frame_skip_counter >= frame_skip)
		{
			skip_frame = 0;
			frame_skip_counter = 0;
		}
		return;
	} else if(frame_skip > 0) {
		skip_frame = 1;
	}

	if(use_drawpixels){
		glRasterPos2i (0, 0);
		glPixelZoom(xscale, yscale);

		if(use_async){
			unsigned int asyncs[2];
			asyncs[0] = async_marker;
			asyncs[1] = 0;
			if(async_first){
				glFinishAsyncSGIX(asyncs);
			} else {
				async_first = 1;
			}
			glAsyncMarkerSGIX(async_marker);
		}

		if(use_ycrcb)
			glDrawPixels(image_width, image_height, GL_YCRCB_422_SGIX,GL_UNSIGNED_BYTE,imagedata);
		else if(use_rgb)
			glDrawPixels(image_width, image_height, GL_RGB,GL_UNSIGNED_BYTE,imagedata);
		else if(use_rgba)
			glDrawPixels(image_width, image_height, GL_RGBA,GL_UNSIGNED_BYTE,imagedata);
		else if(use_abgr)
			glDrawPixels(image_width, image_height, GL_ABGR_EXT,GL_UNSIGNED_BYTE,imagedata);
	} else if(use_textures){
		if(use_ycrcb){
			glXMakeCurrent(mDisplay, _dmpbuffer, _pbcontext);
			glRasterPos2i (0, 0);
			glPixelZoom(1.0, 1.0);
			glDrawPixels(image_width, image_height, GL_YCRCB_422_SGIX,GL_UNSIGNED_BYTE,imagedata);
			glXMakeCurrentReadSGI(mDisplay, vo_window, _dmpbuffer, gl_context);
			glCopyTexSubImage2DEXT(GL_TEXTURE_2D, 0, 0, 0, 0, 0, texture_width, texture_height);
		} else if(use_rendertotex){
			glXMakeCurrent(mDisplay, _dmpbuffer, _pbcontext);
			glRasterPos2i (0, 0);
			glPixelZoom(1.0, -1.0);
			glDrawPixels(image_width, image_height, GL_RGB,GL_UNSIGNED_BYTE,imagedata);
			glXMakeCurrentReadSGI(mDisplay, vo_window, _dmpbuffer, gl_context);
			glCopyTexSubImage2DEXT(GL_TEXTURE_2D, 0, 0, 0, 0, 0, texture_width, texture_height);
		}

		draw_textures();
	}

	glXSwapBuffers( mDisplay,vo_window );

	if (vo_fs && clearcounter){
		glClear (GL_COLOR_BUFFER_BIT);
		clearcounter--;
	}
}

static int draw_slice(uint8_t *src[], int stride[], int w,int h,int x,int y)
{
	int _y = y;
	int dstoff = 0;

//	printf("stride[0]=%d [1]=%d [2]=%d, src[0]%x [1]=%x [2]=%x w=%d h=%d iw=%d\n", stride[0], stride[1], stride[2], src[0], src[1], src[2], w, h, image_width);
	if (skip_frame)
	{
		return 0;
	}

	if(image_format == IMGFMT_YV12) {
		if(use_ycrcb) {
			uint32_t Y1;
			uint32_t U, V;
			uint32_t PY1, PY2;

			dstoff = (image_width<<1)*y;

			uint32_t *dst_1= (uint32_t*)(imagedata + dstoff);
			uint32_t *dst_2= dst_1 + (image_width>>1);
			uint32_t *py_1= src[0];
			uint32_t *py_2= py_1 + (stride[0]>>2);
			uint32_t *pu= (uint32_t*)src[1];
			uint32_t *pv= (uint32_t*)src[2];
			unsigned int h_size;
			uint32_t acc1;

			uint32_t width_diff = (stride[0]+(stride[0]-image_width))>>2;
			uint32_t half_diff = (stride[1]>>2)-(image_width>>3);// (stride[1]+(stride[1]-(image_width>>1)))>>2;
			uint32_t dest_diff = (image_width>>1);

			if(h == 16) {
				UNPACK_TO_UYVY
				UNPACK_TO_UYVY
				UNPACK_TO_UYVY
				UNPACK_TO_UYVY
				UNPACK_TO_UYVY
				UNPACK_TO_UYVY
				UNPACK_TO_UYVY
				UNPACK_TO_UYVY
			} else for(y=0; y<h; y+=2) {
				UNPACK_TO_UYVY
			}
		} else if(use_rgb) {
			dstoff = (image_width*3)*y;
			uint32_t *dst_1= (uint32_t*)(imagedata + dstoff);
			uint32_t *dst_2= dst_1 + ((image_width*3)>>2);
			uint8_t *py_1= src[0];
			uint8_t *py_2= py_1 + stride[0];
			uint8_t *pu= src[1];
			uint8_t *pv= src[2];
			int U, V, Y1, Y2;
			uint32_t acc1;
			uint32_t h_size;

			uint32_t width_diff = (stride[0]+(stride[0]-image_width));
			uint32_t half_diff = (stride[1])-(image_width>>1);
			uint32_t dest_diff = ((image_width*3)>>2)+((image_width&06)>>2);

			if(use_softcs) {
				uint8_t *r, *g, *b;
				uint32_t acc2;

				if(h == 16) {
					UNPACK_TO_RGB
					UNPACK_TO_RGB
					UNPACK_TO_RGB
					UNPACK_TO_RGB
					UNPACK_TO_RGB
					UNPACK_TO_RGB
					UNPACK_TO_RGB
					UNPACK_TO_RGB
				} else for(y=0; y<h; y+=2) {
					UNPACK_TO_RGB
				}
			} else {
				if(h == 16) {
					UNPACK_TO_YUV
					UNPACK_TO_YUV
					UNPACK_TO_YUV
					UNPACK_TO_YUV
					UNPACK_TO_YUV
					UNPACK_TO_YUV
					UNPACK_TO_YUV
					UNPACK_TO_YUV
				} else for(y=0; y<h; y+=2) {
					UNPACK_TO_YUV
				}
			}
		} else if(use_abgr) {
			dstoff = (image_width*4)*y;
			uint32_t *dst_1= (uint32_t*)(imagedata + dstoff);
			uint32_t *dst_2= dst_1 + ((image_width*4)>>2);
			uint8_t *py_1= src[0];
			uint8_t *py_2= py_1 + image_width;
			uint8_t *pu= src[1];
			uint8_t *pv= src[2];
			int U, V, Y1, Y2;
			uint32_t acc1;
			uint32_t h_size;
	
			if(use_softcs){
				uint8_t *r, *g, *b;
				uint32_t acc2;

				for(y=0; y<h; y+=2) {
					UNPACK_TO_ABGR
				}
			} else {
				for(y=0; y<h; y+=2) {
					UNPACK_TO_YUVA
				}
			}
		} else if(use_rgba) {
			dstoff = (image_width*4)*y;
			uint32_t *dst_1= (uint32_t*)(imagedata + dstoff);
			uint32_t *dst_2= dst_1 + ((image_width*4)>>2);
			uint8_t *py_1= src[0];
			uint8_t *py_2= py_1 + stride[0];
			uint8_t *pu= src[1];
			uint8_t *pv= src[2];
			int U, V, Y1, Y2;
			uint32_t acc1;
			uint32_t h_size;

			if(h == 16) {
				UNPACK_TO_YUVA
				UNPACK_TO_YUVA
				UNPACK_TO_YUVA
				UNPACK_TO_YUVA
				UNPACK_TO_YUVA
				UNPACK_TO_YUVA
				UNPACK_TO_YUVA
				UNPACK_TO_YUVA
			} else for(y=0; y<h; y+=2) {
				UNPACK_TO_YUVA
			}
		}

		if(use_textures) {
			if(!use_ycrcb) {
				setupTextureDirtyArea(0, _y, image_width, h);
			}
		}
	}
	return 0;
}

static int query_format(uint32_t format)
{
	image_format = format;

	switch(format) {
	case IMGFMT_UYVY:
		return VFCAP_CSP_SUPPORTED | VFCAP_CSP_SUPPORTED_BY_HW |
			VFCAP_HWSCALE_UP | VFCAP_HWSCALE_DOWN ;
	case IMGFMT_YV12:
		return VFCAP_CSP_SUPPORTED | VFCAP_CSP_SUPPORTED_BY_HW | VFCAP_OSD |
			VFCAP_HWSCALE_UP | VFCAP_HWSCALE_DOWN;
	case IMGFMT_RGB24:
		return VFCAP_CSP_SUPPORTED | VFCAP_CSP_SUPPORTED_BY_HW | VFCAP_OSD |
			VFCAP_HWSCALE_UP | VFCAP_HWSCALE_DOWN;
	}

    return 0;
}

static void uninit(void)
{
	if(use_async) {
		unsigned int asyncs[2];
		asyncs[0] = async_marker;
		asyncs[1] = 0;
		if(async_first) {
			glFinishAsyncSGIX(asyncs);
		}
		glDisable(GL_ASYNC_DRAW_PIXELS_SGIX);
		glDeleteAsyncMarkersSGIX(async_marker, 1);
	}
	glFinish();
	if( !vo_config_count ) return;
  	if(texgrid) {
   		free(texgrid);
    	texgrid = NULL;
  	}
	XFree(gl_vinfo);
	gl_vinfo = NULL;
	glXDestroyContext(mDisplay, gl_context);
	gl_context = 0;
	
  	vo_x11_uninit();
}


static int preinit(const char *arg)
{
	int parse_err = 0;

	skip_frame = 0;
	frame_skip_counter = 0;
	frame_skip = 0;

	force_textures = 0;
	force_drawpixels = 0;
	force_colormatrix = 0;
	force_ycrcb = 0;
	force_rgba = 0;
	force_rgb = 0;
	force_abgr = 0;
	force_async = 0;
	force_pixeltex = 0;
	force_softcs = 0;
	force_rendertotex = 0;

	if(arg) {
		const char *parse_pos = &arg[0];
		while (parse_pos[0] && !parse_err) {
			if (strncmp (parse_pos, "textures", 8) == 0) {
				parse_pos = &parse_pos[8];
				force_textures = 1;
			} else if (strncmp (parse_pos, "drawpixels", 10) == 0) {
				parse_pos = &parse_pos[10];
				force_drawpixels=1;
			} else if (strncmp (parse_pos, "pixeltex", 8) == 0) {
				parse_pos = &parse_pos[8];
				force_pixeltex=1;
			} else if (strncmp (parse_pos, "colormatrix", 11) == 0) {
				parse_pos = &parse_pos[11];
				force_colormatrix=1;
			} else if (strncmp (parse_pos, "ycrcb", 5) == 0) {
				parse_pos = &parse_pos[5];
				force_ycrcb=1;
			} else if (strncmp (parse_pos, "double", 6) == 0) {
				parse_pos = &parse_pos[6];
				double_buffer=1;
			} else if (strncmp (parse_pos, "nodouble", 8) == 0) {
				parse_pos = &parse_pos[8];
				double_buffer=0;
			} else if (strncmp (parse_pos, "rgba", 4) == 0) {
				parse_pos = &parse_pos[4];
				force_rgba = 1;
			} else if (strncmp (parse_pos, "abgr", 4) == 0) {
				parse_pos = &parse_pos[4];
				force_abgr = 1;
			} else if (strncmp (parse_pos, "rgb", 3) == 0) {
				parse_pos = &parse_pos[3];
				force_rgb = 1;
			} else if (strncmp (parse_pos, "async", 5) == 0) {
				parse_pos = &parse_pos[5];
				force_async = 1;
			} else if (strncmp (parse_pos, "softcs", 6) == 0) {
				parse_pos = &parse_pos[6];
				force_softcs = 1;
			} else if (strncmp (parse_pos, "skip1", 5) == 0) {
				parse_pos = &parse_pos[5];
				frame_skip = 1;
			} else if (strncmp (parse_pos, "skip2", 5) == 0) {
				parse_pos = &parse_pos[5];
				frame_skip = 2;
			} else if (strncmp (parse_pos, "skip3", 5) == 0) {
				parse_pos = &parse_pos[5];
				frame_skip = 3;
			} else if (strncmp (parse_pos, "skip4", 5) == 0) {
				parse_pos = &parse_pos[5];
				frame_skip = 4;
			} else {
				mp_msg(MSGT_VO, MSGL_FATAL,
            	"\nvo_sgi command line help:\n\n"
            	"Example: mplayer -vo sgi:option1:option2\n"
            	"\noptions accepted by vo_sgi are:\n\n"
            	"\ttextures:	force drawing via textures\n"
				"\tdrawpixels:	force drawing via glDrawPixels\n"
				"\tdouble:		force double buffered visual\n"
				"\tnodouble:	force single buffered visual\n"
				"\tsoftcs:		force Software colorspace conversion\n"
				"\tpixeltex:	force SGIX/SGIS_pixel_texture colorspace conversion (Impact/VPRO only)\n"
				"\tcolormatrix:	force SGI_color_matrix for hardware colorspace conversion\n"
				"\tycrcb:		force YCrCb hardware colorspace conversion (O2 only)\n"
				"\trgb:			force rgb\n"
				"\trgba:		force rgba\n"
				"\tabgr:		force abgr\n"
				"\tasync:		force SGIX_async_pixel for asynchronous glDrawPixels drawing (VPRO only)\n"
				"\tskip[1-4]:	skip output of 1-4 frames per frame displayed\n"
            	"\n" );
				return -1;
			}
			if(parse_pos[0] == ':') {
				parse_pos = &parse_pos[1];
			} else if(parse_pos[0]) {
				parse_err = 1;
			}
		}
	}

	if(!vo_init()) {
		return -1; // Can't open X11
	}

	return 0;
}


static int control(uint32_t request, void *data)
{
	switch(request) {
	case VOCTRL_PAUSE:
		return (paused=1);
	case VOCTRL_RESUME:
		return (paused=0);
	case VOCTRL_QUERY_FORMAT:
		return query_format(*((uint32_t*)data));
	case VOCTRL_GUISUPPORT:
        return VO_TRUE;
	case VOCTRL_ONTOP:
		vo_x11_ontop();
		return VO_TRUE;
	case VOCTRL_FULLSCREEN:
		vo_x11_fullscreen();
		return VO_TRUE;
	}
	return VO_NOTIMPL;
}
