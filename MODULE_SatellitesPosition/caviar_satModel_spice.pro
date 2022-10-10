;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:caviar_satModel_spice_getPoints
; PURPOSE:返回边缘线/过渡线/赤道线的J2000 ra/dec和图像x/y位置
; INPUT:
;		- radii:椭圆体三轴半径
;		- tipm:将位置向量从惯性参考系转换到body-fixed参考系的变换矩阵
;		- 5:边缘点数量
;		- rho:从目标星体指向观察者的位置向量(J2000)
;		- rhoi:TRANSPOSE(tipm##rho):将rho位置向量从J2000转为body-fixed参考系,同时将向量转置（猜测是计算需要）
;				转置将z轴和x轴的坐标对换
; OUTPUT:{RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
;---------------------------------------------------------------------------------------------------
FUNCTION caviar_satModel_spice_getPoints, radius, tipm, npts, rho, rhoi
	print, '-----------caviar_satModel_spice_getPoints------------'
	; Get CSPICE ellipsoid as viewed from the spacecraft:
	; For limb, rhoi vector ~ sat->spc
	; For terminator, rhoi vector ~ sun->sat
	; For equator, rhoi vector ~ north->sat
	; 获得从航天器看的CSPICE椭圆体:
	; 对于边缘, rhoi向量是以目标星体为原点的航天器的坐标
	; 对于明暗交界线,rhoi向量是以太阳为原点的目标星体的坐标
	; 对于赤道,rhoi向量是以南极为原点的目标星体的坐标
	; cspice_edlimb:计算从一个特定的点看到的椭球的边缘
	; input：
	;	- radius[0]:x轴上的椭球半轴长度
	;	- radius[1]:y 轴上的椭球半轴长度
	;	- radius[2]:z 轴上的椭球半轴长度
	;	- rhoi:body-fixed参考系下观察椭球的点,该点必须在椭球外
	; output:
	;	- spice_ellipse:返回从指定位置看到的椭球的边缘,是一个结构体(椭球的边缘也是一个椭圆),该结构体具有如下几个属性：
	;						    			- center:  dblarr(3),椭圆中心
	;										- semimajor: dblarr(3),椭圆长半轴长度
	;										- semiminor: dblarr(3),椭圆短半轴长度
	cspice_edlimb, radius[0], radius[1], radius[2], rhoi, spice_ellipse
	
	; Extract CSPICE ellipsoid parameters: center, semi-major axis and semi-minor axis
	; 提取Spice椭圆参数：中心、长半轴和短半轴（Body-fixed三维坐标）
	;	- center:椭圆中心
	;	- smajor:长半轴
	;	- sminor:短半轴
	; cspice_el2cgv:将一个SPICE椭圆转换为一个中心向量和两个生成向量,两个生成向量是椭圆的半轴
	; 这个椭圆是以下这些点的集合:center + cos(θ)*smajor + sin(θ)*sminor,其中θ的范围在区间 (-π, π]
	cspice_el2cgv, spice_ellipse, center, smajor, sminor
	print, 'B_center = ', center
	print, 'B_smajor = ', smajor
	print, 'B_sminor = ', sminor

	; Convert ellipsoid parameters from body-fixed (x,y,z)sat to inertial (x,y,z)J2000 coordinates:
	; 将椭圆体参数从以目标体为中心的(x,y,z)坐标转换成J2000 (x,y,z)坐标
	print, 'ell_tipm = '
	print, tipm
	J2000_center = tipm#center
	J2000_smajor = tipm#smajor
	J2000_sminor = tipm#sminor
	print, 'tipm#B_parameters:'
	print, 'J2000_center = ', J2000_center
	print, 'J2000_smajor = ', J2000_smajor
	print, 'J2000_sminor = ', J2000_sminor

	; Construct theta vector from -pi to +pi
	; 构建从-π到+π的θ向量
	; theta = LINDGEN(npts) * 2*!DPI/(npts-1) - !DPI
	delrol = 2*!DPI/npts
	theta = LINDGEN(npts) * delrol
	print, 'θ = ', theta
	
	; Compute ellipsoid points position in inertial coordinates
	; 在惯性坐标J2000(x,y,z)中计算椭圆体边缘点的位置
	J2000_xyz_points = DBLARR(3,npts)
	cos = cos(theta)#J2000_smajor
	sin = sin(theta)#J2000_sminor
	; REPLICATE创建一个n维数组或矩阵,数组和矩阵的值用center[i]填充
	FOR i=0,2 DO J2000_xyz_points[i,*] = REPLICATE(J2000_center[i], npts) + cos[*,i] + sin[*,i]
	print, 'ell_J2000_xyz_points = '
  print, J2000_xyz_points
  
  ; Body-fixed下计算椭圆边缘点位置,再转换为J2000坐标
  
  
	; Compute ellipse point vector
	; 计算椭圆边缘点切向量,椭圆边缘点坐标-航天器坐标
	x = J2000_xyz_points[0,*]-rho[0]
	y = J2000_xyz_points[1,*]-rho[1]
	z = J2000_xyz_points[2,*]-rho[2]
	pvec = [x, y, z]
	
	;***********************************************************************************************
	; Filter ellipse points that are not in the plane normal to pointing vector:
	; 过滤不在指向矢量法线上的椭圆边缘点:
	invNorm_pvec = DBLARR(3,npts)
	FOR i=0L, npts-1 DO invNorm_pvec[*,i] = 1.0D/NORM(pvec[*,i])
	
	points_test = TRANSPOSE(tipm) # (J2000_xyz_points - 0.05*pvec*invNorm_pvec)
	pvec_test = -(TRANSPOSE(tipm) # pvec)
		
	found = INTARR(npts)
	FOR i=0L, npts-1 DO BEGIN
		; https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_surfpt.html
		; cspice_surfpt确定视线向量与椭球表面的交点
		; INPUT:
		;		- points_test[*,i]:一个三维向量,观察者相对于椭球中心的位置,在Body-fixed坐标下表示
		;		- pvec_test[*,i]:从观察者发出的3维指向向量
		;		- radius[0]:椭球半轴长度,平行于body-fixed参考系下的x轴
		; 		- radius[1]:椭球半轴长度,平行于body-fixed参考系下的y轴
		;		- radius[2]:椭球半轴长度,平行于body-fixed参考系下的z轴
		; OUTPUT:
		; 		- fndpnt:从观察者位置发出的指向矢量在椭球表面上的截点位置。
		;		如果射线与椭球相交,“截点”是该射线第一次遇到椭球的地方的Body-fixed坐标
		;		如果不相交,截点将返回(0, 0, 0)
		;		- fndi:一个逻辑标志位,表示来自观察者的射线是否与椭球相交,相交返回TRUE,不相交返回FALSE
		cspice_surfpt, points_test[*,i], pvec_test[*,i], radius[0],radius[1],radius[2], fndpnt, fndi
		found[i] = fndi
	ENDFOR
	index = WHERE(found EQ 0, count)
	IF count GT 0 THEN pvec_fnd = pvec[*,index] ELSE RETURN, 0
	
	;***********************************************************************************************
	; Conversion of ellipse points from (x,y,z)J2000 to (RA,dec)J2000 (radians):
	; 椭圆边缘点从J2000(x,y,z)坐标转换为J2000(RA,dec)坐标（弧度）
	; cspice_recrad:将三维直角坐标转换为赤经,赤纬坐标
	; input:
	; 	- pvec_fnd:一个双精度3元素向量或3xN数组,包含一个位置或一组位置的三维直角坐标
	; output:
	;	- range:一个双精度标量或具有N个元素的数组，描述了对应的位置与原点的距离
	;	- ra:一个双精度标量或N个元素的数组,描述位置的赤经,单位为弧度
	;	- dec:一个双精度标量或N个元素的数组,描述位置的纬经,单位为弧度
	cspice_recrad, pvec_fnd, range, ra, dec
		
	; Conversion of ellipse points from (RA,dec)J2000 to (sample,line):
	; 椭圆边缘点从J2000(RA,dec)坐标转换为(sample,line)坐标
	; radec2slcoord:将RA/Dec坐标转换为图像x,y坐标
	radec2slcoord, ra, dec, sample, line
			
	; Return ellipse points coordinates in ra/dec (milli-arcseconds) and sample/line coordinates:
	; 返回椭圆边缘点的ra/dec坐标（毫弧）和图像x,y坐标
	RETURN, {RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_satModel_spice
; PURPOSE: 从SPICE内核加载卫星形状模型
; INPUT: 
; - satID：   卫星id，表示要加载哪颗卫星的模型
; - et：      图像曝光时间中间点
; - spcID：   航天器ID
; - n_points：模型边缘点数量
; - CENTER：  模型中心
; OUTPUT:
; - model：   目标体模型结构体,包含目标体的中心坐标、边缘坐标、明暗线坐标、赤道线坐标
;---------------------------------------------------------------------------------------------------
FUNCTION caviar_satModel_spice, satID, et, spcID, n_points, CENTER=center
  print, '-----------caviar_satModel_spice------------'
  print, 'spcID = ', spcID
  print, 'satID = ', satID
  
  ; cspice_bodc2n将对象ID转换为对象名称
  cspice_bodc2n, spcID, spc_name, found
  if found then begin
    print, 'spc_name = ', spc_name
  endif
  cspice_bodc2n, satID, sat_name, found
  if found then begin
	print, 'sat_name = ', sat_name
  endif
  
  IF n_points LE 0 THEN MESSAGE, "Variable 'NPTS' must be greater than zero"
  
  ;# Get sat->spc position vector
  ; 获得sat->spc的位置向量(J2000)
  ; CSPICE_SPKEZ返回目标体相对于观察者的状态（位置和速度）
  ; https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_spkez.html
  ; Input：
  ; 	- satID：目标星体ID,目标星体和观察者之间的位置向量从观察者指向目标星体
  ; 	- et:星历表时间,表示为J2000TDB之后的秒数,在此时间计算目标体相对于观测者的状态,'et'表示观测者所在位置的时间
  ; 	- 'J2000':参考框架的名称,输出的状态矢量应相对于该参考框架表示
  ; 	- 'CN+S':一个标量字符串,表示应用于目标体状态的像差校正方式,以考虑到单向光照时间和恒星像差
  ; 	- spcID:观测主体的ID
  ; Output:
  ; 	- state: 6维向量,表示目标物体相对于指定观察者的位置（以千米为单位）和以千米/秒为单位的速度
  ;     （state的前三个分量代表目标体位置的x、y和z分量;后三个分量构成相应的速度向量）
  ; 	- ltime:观察者和目标之间的单向光照时间,单位为秒
  cspice_spkez, satID, et, 'J2000', 'CN+S', spcID, state, ltime
  rho=-state[0:2] ; 观察者相对于目标星体的位置向量(J2000)
  rho_center=rho  ; rho_center即为从观察者相对于目标体的位置向量(J2000)
  
  ; debug
  length_sat_to_spc = NORM(rho)
  print, 'length_sat_to_spc = ', length_sat_to_spc
  help, rho
  print, 'rho = ', rho

  ; KEYWORD_SET函数根据指定表达式的值返回一个字节值,如果其参数已定义且非零，则返回1（真），否则返回0（假）
  ; center是CAVIAR_DATA公共块image结构体中的一个成员变量
  IF KEYWORD_SET(center) THEN BEGIN
    ; slcoord2radec是自定义过程,将图像的X/Y坐标转换为RA/DEC坐标
    ; INPUT:含有像素的X/Y坐标的标量或数组
    ; 可选输入参数:/ITERATE
    ; OUTPUT:包含RA/Dec坐标的标量或数组
    slcoord2radec, center[0], center[1], RA, dec, /ITERATE  ; 将图像的中心x/y坐标转换为赤经、赤纬坐标

    ; NORM函数计算向量或二维数组的范数,默认NORM对向量计算二范数,对数组计算0范数
    rho = - NORM(rho) * [COS(dec)*COS(RA), COS(dec)*SIN(RA), SIN(dec)]
    rho_center=-rho ; 目标物体中心的赤经赤纬坐标
  ENDIF

  ;# Get sun->sat position vector
  ; 获得太阳指向目标星体的位置向量(J2000)
  cspice_spkez, 10L, et-ltime, 'J2000', 'CN', satID, state, ltime_sun
  rho_sun=-state[0:2] ; 目标星体指向太阳的位置向量取反,获得太阳指向目标星体的位置向量
  
  ; debug
  length_sun_to_sat = NORM(rho_sun)
  print, 'length_sun_to_sat = ', length_sun_to_sat


  ;# Get 3x3 transformation matrix from inertial (J2000) to body-equator-and-prime-meridian coordinates: (tipm)
  ; 获得从惯性参考系(J2000)到body-fixed参考系的3x3变换矩阵:(tipm)
  ; cspice_tipbod返回将位置向量从惯性参考系转换到body-fixed参考系的变换矩阵
  ; Input:
  ; 	- 'J2000':惯性参考系名称
  ;		- satID:星体ID,body-fixed参考系对应的星体ID(satID和Body-fixed参考系ID不是一样的!!!)
  ;		- et: 位置变换请求所在的时间,通常是观察时间减去从观察者到物体的单向光时间
  ; Output:
  ;		- tipm:转换矩阵
  ; https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_tipbod.html
  cspice_tipbod, 'J2000', satID, et-ltime, tipm

  ;# Get length of ellipsoid semi-axis lying on the x-axis/y-axis/z-axis
  ; 获取位于x轴/y轴/z轴上的椭圆体半轴的长度
  ; cspice_bodvar返回一个双精度向量,包含存储在内核池中的一些指定主体数据的值。
  ; Input:
  ; 	- satID:目标主体的ID
  ; 	- 'RADII':指定要返回的属性
  ; Output:
  ; - radii:目标主体的半径,双精度3维数组
  cspice_bodvar, satID, 'RADII', radii

  ;# Compute limb/terminator/equator ra/dec & x/y positions
  ; caviar_satModel_spice_getPoints返回边缘线/过渡线/赤道线的ra/dec和x/y位置
  ; INPUT:
  ;		- radii:椭圆体三轴半径
  ;		- tipm:将位置向量从惯性参考系转换到body-fixed参考系的变换矩阵
  ;		- 5:边缘点数量
  ;		- rho:从目标星体指向观察者的位置向量(J2000)
  ;		- rhoi:将rho位置向量从J2000转为body-fixed参考系，同时将列向量转为行向量
  ; OUTPUT:{RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
  ; TRANSPOSE对矩阵或数组进行转置,这里将列向量转为行向量,因为cspice_edlimb要求传入的是行向量(三维数组)
  rhoi = TRANSPOSE(tipm##rho)
  limb = caviar_satModel_spice_getPoints(radii, tipm, 2, rho, rhoi)
  print, 'ell_limb = '
  print, limb


  ; term = caviar_satModel_spice_getPoints(radii, tipm, 2, rho, TRANSPOSE(tipm##rho_sun))
  ; equa = caviar_satModel_spice_getPoints(radii, tipm, 2, rho, [0.0D, 0.0D, 300000.0D])


  ;# Compute limb center ra/dec & x/y position from rectangular coordinates (rho_center)
  ; 计算三维直角坐标下物体中心的ra/dec和x/y位置（rho_center）
  cspice_recrad, rho_center, range, rac, decc
  radec2slcoord, rac, decc, sc, lc

  model = {npts: n_points, slCenter: [[sc],[lc]], rdCenter: [[rac],[decc]], $
    slLimb:[[limb.sample], [limb.line]], rdLimb:[[limb.ra], [limb.dec]], $
    slTerm:[[0], [0]], rdTerm:[[0], [0]], $
    slEqua:[[0], [0]], rdEqua:[[0], [0]] $
  }

  RETURN, model
END

; @孙德淦，2022.10.2,从SPICE内核加载DSK模型的函数
FUNCTION caviar_satModel_spice_dsk, satID, et, spcID, n_points
	print, 'DSK:'

	IF n_points LE 0 THEN MESSAGE, "Variable 'NPTS' must be greater than zero"

	cspice_spkez, satID, et, 'J2000', 'CN+S', spcID, state, ltime
	rho=-state[0:2]	
	rho_center=rho	

	; 判断center变量是否已经定义且非零值,center是CAVIAR_DATA公共块image结构体中的一个成员变量
	IF KEYWORD_SET(center) THEN BEGIN
		; slcoord2radec是自定义过程,将图像的X/Y坐标转换为RA/Dec坐标
		; Input:含有像素的X/Y坐标的标量或数组
		; 可选输入参数:/ITERATE
		; Output:包含RA/Dec坐标（单位:弧度）的标量或数组
		slcoord2radec, center[0], center[1], RA, dec, /ITERATE	; 将图像的中心x/y坐标转换为赤经、赤纬坐标

		; NORM函数计算向量或二维数组的范数,默认NORM对向量计算二范数,对数组计算0范数
		rho = - NORM(rho) * [cos(dec)*cos(RA), cos(dec)*sin(RA), sin(dec)]	; 目标物体相对于航天器的dec,ra位置坐标
		rho_center=-rho	; 航天器相对于目标物体的dec,ra位置
	ENDIF
	
	; 设置cspice_limbpt函数的参数
	MAXN = 2	; 可以存储的最大边缘点个数
	method = 'TANGENT/DSK/UNPRIORITIZED'
	z = [ 0.D, 0.0, 1.0 ]
	str_spcID = string(spcID)
	str_satID = string(satID)
	obsrvr = strtrim(str_spcID)
	target = strtrim(str_satID)
	print, 'obsrvr_ID = ', obsrvr
	print, 'target_ID = ', target
	; fixref的名称格式为:IAU_body name,例如:火卫一,IAU_PHOBOS; 土卫九,IAU_PHOEBE
	fixref = 'IAU_DIONE'

	; cspice_namfrm将参考系名称转换为对应的参考系ID
	cspice_namfrm, fixref, fixref_id
	print, 'IAU_DIONE_ID = ', fixref_id
	print, 'satID = ', satID

	abcorr = 'CN+S'
	corloc = 'CENTER'
	schstp = 1.0d-4	    ; 搜索角度步长,100微弧
	soltol = 1.0d-7	    ; 收敛角度,100纳弧
	ncuts  = 2		; 切割半平面的个数
	delrol = cspice_twopi() / ncuts
	; 获得DSK模型的边缘点坐标，基于Body-fixed参考系
	cspice_limbpt, method, $
				target, et,     fixref,            $
				abcorr,    corloc, obsrvr, z,      $
				delrol,    ncuts,  schstp, soltol, $
				MAXN, npts, points, trgeps, tangts
   
	print, 'dsk_body-fixed_points = '
	print, points
	

	; 将边缘点坐标（Body-fix）转换为J2000(x,y,z)坐标
	; 首先得到转换矩阵tipm,可以调用下面两个方法：
	; - CSPICE_PXFORM:返回在指定时间内将位置向量从一个指定参考系转换到另一个指定参考系的矩阵
	; - cspice_tipbod:计算将惯性参考系中的位置向量转换到Body-fixed参考系的3x3变换矩阵tipm
;	cspice_tipbod, 'J2000', fixref_id, et-ltime, tipm
;	; 对tipm求逆,获得从Body—fixed参考系转换为J2000参考系的变换矩阵
;	cspice_tipbod_tran_mat = invert(tipm)
;	; 打印satID对应的参考系名称,检查是否和fixref一致
;	cspice_frmnam, fixref_id, body_fixed_name
;	print, 'body_fixed_name = ', body_fixed_name
;	print, 'cspice_tipbod_tran_mat = ', cspice_tipbod_tran_mat
;
;	; 使用cspice_tipbod_tran_mat将Body-fixed (x,y,z)坐标转换成J2000 (x,y,z)坐标
;	; https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_tipbod.html#Examples
;	j2000_xyz_points_tipbod = dblarr(3, MAXN)
;	start = 0
;	FOR j = 0, ncuts-1 DO BEGIN
;		FOR k = 0, npts[j]-1 DO BEGIN
;			temp = points[*, k+start]
;			j2000_xyz_points_tipbod[*, k+start] = cspice_tipbod_tran_mat##temp
;		ENDFOR
;		start = start + npts[j]
;	ENDFOR
;    print, 'dsk_J2000_xyz_points_tipbod = '
;    print, j2000_xyz_points_tipbod

	; CSPICE_PXFORM返回将位置向量从Body—fixed参考系转换为J2000参考系的变换矩阵rotate
	cspice_pxform, fixref, 'J2000', et-ltime, rotate
	cspice_pxform_tran_mat = transpose(rotate)
	print, 'cspice_pxform_tran_mat = ', cspice_pxform_tran_mat

	; 使用cspice_pxform_tran_mat将Body-fixed (x,y,z)坐标转换成J2000 (x,y,z)坐标
	; https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_pxform.html#Examples
	j2000_xyz_points_pxform = dblarr(3, MAXN)
	start = 0
	FOR j = 0, ncuts-1 DO BEGIN
		FOR k = 0, npts[j]-1 DO BEGIN
			temp = points[*, k+start]
			j2000_xyz_points_pxform[*, k+start] = cspice_pxform_tran_mat#temp
		ENDFOR
		start = start + npts[j]
	ENDFOR
  	print, 'dsk_J2000_xyz_points_pxform = '
	print, j2000_xyz_points_pxform

	; 将边缘点J2000(x,y,z)坐标转换为J2000(RA,dec)坐标
	; cspice_recrad:将三维直角坐标转换为赤经,赤纬坐标
	; input:
	; 	- j2000_xyz_points_tipbod:一个双精度3元素向量或3xN数组,包含一个位置或一组位置的三维直角坐标
	; output:
	;	- range:一个双精度标量或具有N个元素的数组，描述了对应的位置与原点的距离
	;	- ra:一个双精度标量或N个元素的数组,描述位置的赤经,单位为弧度
	;	- dec:一个双精度标量或N个元素的数组,描述位置的纬经,单位为弧度
	cspice_recrad, j2000_xyz_points_pxform, range, ra, dec
  
	; 将边缘点J2000 RA/Dec坐标转换为图像x,y坐标
	radec2slcoord, ra, dec, sample, line

	; 将目标体中心的J2000(x,y,z)坐标转换为J2000 RA/DEC坐标
	cspice_recrad, rho_center, range, rac, decc

	; 将J2000 RA/DEC坐标转换为图像x/y坐标
	radec2slcoord, rac, decc, sc, lc
	
	; 边缘模型结构体，记载了DSK模型边缘点的有关数据
	limb = {RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
	PRINT, 'dsk_limb = '
	PRINT, limb
	
  model = {npts: 2, slCenter: [[sc],[lc]], rdCenter: [[rac],[decc]], $
    slLimb:[[limb.sample], [limb.line]], rdLimb:[[limb.ra], [limb.dec]], $
    slTerm:[[0], [0]], rdTerm:[[0], [0]], $
    slEqua:[[0], [0]], rdEqua:[[0], [0]] $
  }

	RETURN, model
END