////////////////////////////////////////////////////////
/*              UNPACK YV12 DATA to UYVY              */
////////////////////////////////////////////////////////
#define UNPACK_TO_UYVY \
				h_size = 1+((image_width)>>3);				\
				while (--h_size) {							\
					U = *pu; V = *pv;						\
					PY1 = py_1[0]; PY2 = py_2[0];			\
					acc1 = (U&0xff000000)|((V>>16)&0xff00);	\
\
					Y1 = (PY1>>8)&0x00ff0000;				\
					dst_1[0] = acc1|(Y1)|((PY1>>16)&0x00ff);\
					Y1 = (PY2>>8)&0x00ff0000;				\
					dst_2[0] = acc1|(Y1)|((PY2>>16)&0x00ff);\
\
					U <<= 8;								\
					acc1 = (U&0xff000000)|((V>>8)&0xff00);	\
\
					Y1 = (PY1<<8)&0x00ff0000;				\
					dst_1[1] = acc1|(Y1)|(PY1&0x00ff);		\
					Y1 = (PY2<<8)&0x00ff0000;				\
					dst_2[1] = acc1|(Y1)|(PY2&0x00ff);		\
\
					U <<= 8;								\
					acc1 = (U&0xff000000)|(V&0xff00);		\
\
					PY1 = py_1[1]; PY2 = py_2[1];			\
					Y1 = (PY1>>8)&0x00ff0000;				\
					dst_1[2] = acc1|(Y1)|((PY1>>16)&0x00ff);\
					Y1 = (PY2>>8)&0x00ff0000;				\
					dst_2[2] = acc1|(Y1)|((PY2>>16)&0x00ff);\
\
					acc1 = (U<<8)|((V<<8)&0xff00);			\
\
					Y1 = (PY1<<8)&0x00ff0000;				\
					dst_1[3] = acc1|(Y1)|(PY1&0x00ff);		\
					Y1 = (PY2<<8)&0x00ff0000;				\
					dst_2[3] = acc1|(Y1)|(PY2&0x00ff);		\
\
					pu++;									\
					pv++;									\
					py_1 += 2;								\
					py_2 += 2;								\
					dst_1 += 4;								\
					dst_2 += 4;								\
				}\
				dst_1 += dest_diff; dst_2 += dest_diff;		\
				py_1 += width_diff; py_2 += width_diff; 	\
				pu += half_diff; pv += half_diff;



////////////////////////////////////////////////////////
/*              UNPACK YV12 DATA to YUV               */
////////////////////////////////////////////////////////
#define UNPACK_TO_YUV \
			h_size = image_width>>3;\
			while (h_size--) {\
				U = pu[0];\
				V = pv[0];\
				Y1 = py_1[0];\
				Y2 = py_1[1];\
				acc1 = (U<<16)|(V<<8);\
				dst_1[0]=(Y1<<24)|acc1|Y2;\
				Y1 = py_2[0];\
				Y2 = py_2[1];\
				dst_2[0]=(Y1<<24)|acc1|Y2;\
				acc1 <<= 8;\
\
				U = pu[1];\
				V = pv[1];\
				acc1 |= U;\
				Y1 = py_1[2];\
				Y2 = py_2[2];\
				dst_1[1]=acc1|(Y1<<8);\
				dst_2[1]=acc1|(Y2<<8);\
				acc1 <<= 8;\
				acc1 |= V;\
				Y1 = py_1[3];\
				Y2 = py_2[3];\
				dst_1[2]=acc1|(Y1<<16);\
				dst_2[2]=acc1|(Y2<<16);\
\
				U = pu[2];\
				V = pv[2];\
				Y1 = py_1[4];\
				Y2 = py_1[5];\
				acc1 = (U<<16)|(V<<8);\
				dst_1[3]=(Y1<<24)|acc1|Y2;\
				Y1 = py_2[4];\
				Y2 = py_2[5];\
				dst_2[3]=(Y1<<24)|acc1|Y2;\
				acc1 <<= 8;\
\
				U = pu[3];\
				V = pv[3];\
				acc1 |= U;\
				Y1 = py_1[6];\
				Y2 = py_2[6];\
				dst_1[4]=acc1|(Y1<<8);\
				dst_2[4]=acc1|(Y2<<8);\
				acc1 <<= 8;\
				acc1 |= V;\
				Y1 = py_1[7];\
				Y2 = py_2[7];\
				dst_1[5]=acc1|(Y1<<16);\
				dst_2[5]=acc1|(Y2<<16);\
\
				pu += 4;\
				pv += 4;\
				py_1 += 8;\
				py_2 += 8;\
				dst_1 += 6;\
				dst_2 += 6;\
			}\
			dst_1 += ((image_width*3)>>2);\
			dst_2 += ((image_width*3)>>2);\
			py_1 += image_width;\
			py_2 += image_width;



////////////////////////////////////////////////////////
/*              UNPACK YV12 DATA to YUVA              */
////////////////////////////////////////////////////////
#define UNPACK_TO_YUVA \
			h_size = image_width>>3;\
			while (h_size--) {\
				U = pu[0];\
				V = pv[0];\
				acc1 = (U<<16)|(V<<8)|0xff;\
				Y1 = py_1[0];\
				Y2 = py_1[1];\
				dst_1[0] = (Y1<<24)|acc1;\
				dst_1[1] = (Y2<<24)|acc1;\
				Y1 = py_2[0];\
				Y2 = py_2[1];\
				dst_2[0] = (Y1<<24)|acc1;\
				dst_2[1] = (Y2<<24)|acc1;\
\
				U = pu[1];\
				V = pv[1];\
				acc1 = (U<<16)|(V<<8)|0xff;\
				Y1 = py_1[2];\
				Y2 = py_1[3];\
				dst_1[2] = (Y1<<24)|acc1;\
				dst_1[3] = (Y2<<24)|acc1;\
				Y1 = py_2[2];\
				Y2 = py_2[3];\
				dst_2[2] = (Y1<<24)|acc1;\
				dst_2[3] = (Y2<<24)|acc1;\
\
				U = pu[2];\
				V = pv[2];\
				acc1 = (U<<16)|(V<<8)|0xff;\
				Y1 = py_1[4];\
				Y2 = py_1[5];\
				dst_1[4] = (Y1<<24)|acc1;\
				dst_1[5] = (Y2<<24)|acc1;\
				Y1 = py_2[4];\
				Y2 = py_2[5];\
				dst_2[4] = (Y1<<24)|acc1;\
				dst_2[5] = (Y2<<24)|acc1;\
\
				U = pu[3];\
				V = pv[3];\
				acc1 = (U<<16)|(V<<8)|0xff;\
				Y1 = py_1[6];\
				Y2 = py_1[7];\
				dst_1[6] = (Y1<<24)|acc1;\
				dst_1[7] = (Y2<<24)|acc1;\
				Y1 = py_2[6];\
				Y2 = py_2[7];\
				dst_2[6] = (Y1<<24)|acc1;\
				dst_2[7] = (Y2<<24)|acc1;\
\
				pu += 4;\
				pv += 4;\
				py_1 += 8;\
				py_2 += 8;\
				dst_1 += 8;\
				dst_2 += 8;\
			}\
\
			dst_1 += (image_width);\
			dst_2 += (image_width);\
			py_1 += image_width;\
			py_2 += image_width;

////////////////////////////////////////////////////////
/*              UNPACK YV12 DATA to AVUY              */
////////////////////////////////////////////////////////
#define UNPACK_TO_AVUY \
			h_size = image_width>>1;\
			while (h_size--) {\
				U = pu[0];\
				V = pv[0];\
				acc1 = 0xff000000|(U<<16)|(V<<8);\
				Y1 = py_1[0];\
				Y2 = py_1[1];\
				dst_1[0] = acc1|Y1;\
				dst_1[1] = acc1|Y2;\
				Y1 = py_2[0];\
				Y2 = py_2[1];\
				dst_2[0] = acc1|Y1;\
				dst_2[1] = acc1|Y2;\
\
				pu ++;\
				pv ++;\
				py_1 += 2;\
				py_2 += 2;\
				dst_1 += 2;\
				dst_2 += 2;\
			}\
\
			dst_1 += image_width;\
			dst_2 += image_width;\
			py_1 += image_width;\
			py_2 += image_width;


////////////////////////////////////////////////////////
/*              UNPACK YV12 DATA to RGB               */
////////////////////////////////////////////////////////
#define UNPACK_TO_RGB \
		h_size= image_width>>3;\
\
		while (h_size--) {\
			U = pu[0];\
			V = pv[0];\
			r = (uint8_t*)table_rV[V];\
			g = (uint8_t*)table_gU[U] + table_gV[V];\
			b = (uint8_t*)table_bU[U];\
			Y1 = py_1[0];\
			Y2 = py_1[1];\
			acc1 =  r[Y1]<<24;\
			acc1 += g[Y1]<<16;\
			acc1 += b[Y1]<<8;\
			acc1 += r[Y2];\
			dst_1[0]=acc1;\
			acc1 =  g[Y2]<<24;\
			acc1 += b[Y2]<<16;\
			Y1 = py_2[0];\
			Y2 = py_2[1];\
			acc2 =  r[Y1]<<24;\
			acc2 += g[Y1]<<16;\
			acc2 += b[Y1]<<8;\
			acc2 += r[Y2];\
			dst_2[0]=acc2;\
			acc2 =  g[Y2]<<24;\
			acc2 += b[Y2]<<16;\
\
			U = pu[1];\
			V = pv[1];\
			r = (uint8_t*)table_rV[V];\
			g = (uint8_t*)table_gU[U] + table_gV[V];\
			b = (uint8_t*)table_bU[U];\
			Y1 = py_1[2];\
			Y2 = py_1[3];\
			acc1 += r[Y1]<<8;\
			acc1 += g[Y1];\
			dst_1[1]=acc1;\
			acc1 =  b[Y1]<<24;\
			acc1 += r[Y2]<<16;\
			acc1 += g[Y2]<<8;\
			acc1 += b[Y2];\
			dst_1[2]=acc1;\
			Y1 = py_2[2];\
			Y2 = py_2[3];\
			acc2 += r[Y1]<<8;\
			acc2 += g[Y1];\
			dst_2[1]=acc2;\
			acc2 =  b[Y1]<<24;\
			acc2 += r[Y2]<<16;\
			acc2 += g[Y2]<<8;\
			acc2 += b[Y2];\
			dst_2[2]=acc2;\
\
			U = pu[2];\
			V = pv[2];\
			r = (uint8_t*)table_rV[V];\
			g = (uint8_t*)table_gU[U] + table_gV[V];\
			b = (uint8_t*)table_bU[U];\
			Y1 = py_1[4];\
			Y2 = py_1[5];\
			acc1 =  r[Y1]<<24;\
			acc1 += g[Y1]<<16;\
			acc1 += b[Y1]<<8;\
			acc1 += r[Y2];\
			dst_1[3]=acc1;\
			acc1 =  g[Y2]<<24;\
			acc1 += b[Y2]<<16;\
			Y1 = py_2[4];\
			Y2 = py_2[5];\
			acc2 =  r[Y1]<<24;\
			acc2 += g[Y1]<<16;\
			acc2 += b[Y1]<<8;\
			acc2 += r[Y2];\
			dst_2[3]=acc2;\
			acc2 =  g[Y2]<<24;\
			acc2 += b[Y2]<<16;\
\
			U = pu[3];\
			V = pv[3];\
			r = (uint8_t*)table_rV[V];\
			g = (uint8_t*)table_gU[U] + table_gV[V];\
			b = (uint8_t*)table_bU[U];\
			Y1 = py_1[6];\
			Y2 = py_1[7];\
			acc1 += r[Y1]<<8;\
			acc1 += g[Y1];\
			dst_1[4]=acc1;\
			acc1 =  b[Y1]<<24;\
			acc1 += r[Y2]<<16;\
			acc1 += g[Y2]<<8;\
			acc1 += b[Y2];\
			dst_1[5]=acc1;\
			Y1 = py_2[6];\
			Y2 = py_2[7];\
			acc2 += r[Y1]<<8;\
			acc2 += g[Y1];\
			dst_2[4]=acc2;\
			acc2 =  b[Y1]<<24;\
			acc2 += r[Y2]<<16;\
			acc2 += g[Y2]<<8;\
			acc2 += b[Y2];\
			dst_2[5]=acc2;\
\
			pu += 4;\
			pv += 4;\
			py_1 += 8;\
			py_2 += 8;\
			dst_1 += 6;\
			dst_2 += 6;\
		}\
		dst_1 += ((image_width*3)>>2);\
		dst_2 += ((image_width*3)>>2);\
		py_1 += image_width;\
		py_2 += image_width;



////////////////////////////////////////////////////////
/*              UNPACK YV12 DATA to RGBA              */
////////////////////////////////////////////////////////
#define UNPACK_TO_RGBA \
		h_size= image_width>>1;\
\
		while (h_size--) {\
			U = pu[0];\
			V = pv[0];\
			r = (uint8_t*)table_rV[V];\
			g = (uint8_t*)table_gU[U] + table_gV[V];\
			b = (uint8_t*)table_bU[U];\
			Y1 = py_1[0];\
			Y2 = py_1[1];\
			acc1 =  (r[Y1]<<24)|0xff;\
			acc1 += g[Y1]<<16;\
			acc1 += b[Y1]<<8;\
			dst_1[0]=acc1;\
			acc1 =  (r[Y1]<<24)|0xff;\
			acc1 += g[Y2]<<16;\
			acc1 += b[Y2]<<8;\
			dst_1[1]=acc1;\
			Y1 = py_2[0];\
			Y2 = py_2[1];\
			acc2 =  (r[Y1]<<24)|0xff;\
			acc2 += g[Y1]<<16;\
			acc2 += b[Y1]<<8;\
			dst_2[0]=acc2;\
			acc2 =  (r[Y1]<<24)|0xff;\
			acc2 += g[Y2]<<16;\
			acc2 += b[Y2]<<8;\
			dst_2[1]=acc2;\
\
			pu ++;\
			pv ++;\
			py_1 += 2;\
			py_2 += 2;\
			dst_1 += 2;\
			dst_2 += 2;\
		}\
		dst_1 += image_width;\
		dst_2 += image_width;\
		py_1 += image_width;\
		py_2 += image_width;

////////////////////////////////////////////////////////
/*              UNPACK YV12 DATA to ABGR              */
////////////////////////////////////////////////////////
#define UNPACK_TO_ABGR \
		h_size= image_width>>1;\
\
		while (h_size--) {\
			U = pu[0];\
			V = pv[0];\
			r = (uint8_t*)table_rV[V];\
			g = (uint8_t*)table_gU[U] + table_gV[V];\
			b = (uint8_t*)table_bU[U];\
			Y1 = py_1[0];\
			Y2 = py_1[1];\
			acc1 =  0xff000000|(b[Y1]<<16);\
			acc1 += g[Y1]<<8;\
			acc1 += r[Y1];\
			dst_1[0]=acc1;\
			acc1 =  0xff000000|(b[Y2]<<16);\
			acc1 += g[Y2]<<8;\
			acc1 += r[Y2];\
			dst_1[1]=acc1;\
			Y1 = py_2[0];\
			Y2 = py_2[1];\
			acc2 =  0xff000000|(b[Y1]<<16);\
			acc2 += g[Y1]<<8;\
			acc2 += r[Y1];\
			dst_2[0]=acc2;\
			acc2 =  0xff000000|(b[Y2]<<16);\
			acc2 += g[Y2]<<8;\
			acc2 += r[Y2];\
			dst_2[1]=acc2;\
\
			pu ++;\
			pv ++;\
			py_1 += 2;\
			py_2 += 2;\
			dst_1 += 2;\
			dst_2 += 2;\
		}\
		dst_1 += image_width;\
		dst_2 += image_width;\
		py_1 += image_width;\
		py_2 += image_width;
