;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:caviar_satModel_spice_getPoints
; PURPOSE:返回边缘线/过渡线/赤道线的J2000 ra/dec和图像x/y位置
; INPUT:
;		- radii:椭圆体三轴半径
;		- tipm:将位置向量从惯性参考系转换到body-fixed参考系的变换矩阵
;		- npts:边缘点数量
;		- rho:从目标星体指向观察者的位置向量(J2000)
;		- rhoi:body-fixed参考系下观察椭球的点,该点必须在椭球外
; OUTPUT:{RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
;---------------------------------------------------------------------------------------------------
FUNCTION caviar_satModel_spice_getPoints, radius, tipm, npts, rho, rhoi
  print
	print, '-----------------caviar_satModel_spice_getPoints--------------------'
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
	
	; 提取Spice椭圆参数：中心、长半轴和短半轴（Body-fixed三维坐标）
	;	- center:椭圆中心
	;	- smajor:长半轴
	;	- sminor:短半轴
	; cspice_el2cgv:将一个SPICE椭圆转换为一个中心向量和两个生成向量,两个生成向量是椭圆的半轴
	; 这个椭圆是以下这些点的集合:center + cos(θ)*smajor + sin(θ)*sminor,其中θ的范围在区间 (-π, π]
	cspice_el2cgv, spice_ellipse, center, smajor, sminor

	; 将椭圆参数从Body-fixed(x,y,z)参考系转换成J2000 (x,y,z)参考系
	J2000_center = tipm#center
	J2000_smajor = tipm#smajor
	J2000_sminor = tipm#sminor
	
	; 构建从-π到+π的θ向量
	; theta = LINDGEN(npts) * 2*!DPI/(npts-1) - !DPI
	delrol = 2*!DPI/npts
	theta = LINDGEN(npts) * delrol

	; 在惯性参考系J2000(x,y,z)中计算椭圆体边缘点的位置
	J2000_xyz_points = DBLARR(3,npts)
	cos = cos(theta)#J2000_smajor
	sin = sin(theta)#J2000_sminor

	; 将cos和sin中的列向量变成行向量,J2000_xyz_points中的一行表示一个边缘点在J2000参考系下的位置
	FOR i=0,2 DO J2000_xyz_points[i,*] = REPLICATE(J2000_center[i], npts) + cos[*,i] + sin[*,i]

	; 计算J2000下椭圆边缘点的切向量=椭圆边缘点坐标-航天器坐标（切向量由航天器指向边缘点）
	x = J2000_xyz_points[0,*]-rho[0]
	y = J2000_xyz_points[1,*]-rho[1]
	z = J2000_xyz_points[2,*]-rho[2]
	pvec = [x, y, z] ; J2000参考系下的切向量位置,从航天器指向目标星体

	;***********************************************************************************************
	; Filter ellipse points that are not in the plane normal to pointing vector:
	; 过滤不在指向向量法平面上的椭圆边缘点
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
		;		   如果射线与椭球相交,“截点”是该射线第一次遇到椭球的地方的Body-fixed坐标
		;		   如果不相交,截点将返回(0, 0, 0)
		;		- fndi:一个逻辑标志位,表示来自观察者的射线是否与椭球相交,相交返回TRUE,不相交返回FALSE
		cspice_surfpt, points_test[*,i], pvec_test[*,i], radius[0],radius[1],radius[2], fndpnt, fndi
		found[i] = fndi
	ENDFOR
	index = WHERE(found EQ 0, count)
	IF count GT 0 THEN pvec_fnd = pvec[*,index] ELSE RETURN, 0 ; pvec_fnd是过滤完之后的边缘点切向量



	;***********************************************************************************************
	; 将椭圆边缘点（由航天器指向边缘点的切向量定义）从J2000(x,y,z)坐标转换为J2000(RA,dec)坐标（弧度）
	; cspice_recrad:将J2000三维坐标转换为赤经,赤纬坐标
	; input:
	; 	- pvec_fnd:一个双精度3元素向量或3xN数组,（）
	; output:
	;	- range:一个双精度标量或具有N个元素的数组，描述了对应的位置与原点的距离
	;	- ra:一个双精度标量或N个元素的数组,描述位置的赤经,单位为弧度
	;	- dec:一个双精度标量或N个元素的数组,描述位置的纬经,单位为弧度
	print, 'pvec_fnd = '
	print, pvec_fnd
	cspice_recrad, pvec_fnd, range, ra, dec
		
	; 椭圆边缘点从J2000(RA,dec)坐标转换为(sample,line)坐标
	; radec2slcoord:将RA/Dec坐标转换为图像x,y坐标
	radec2slcoord, ra, dec, sample, line
			
	; 返回椭圆边缘点的ra/dec坐标（毫弧）和sample/line坐标
	RETURN, {RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
END


FUNCTION caviar_satModel_spice_ell_getLimbPoints, spcID, satID, n_points, et, ltime
  print
  print, '-----------------caviar_satModel_spice_ell_getLimbPoints--------------------'
  
	; 设置cspice_limbpt函数的参数
	MAXN = n_points	; 可以存储的最大边缘点个数
	method = 'TANGENT/ELLIPSOID'
	z = [ 0.D, 0.0, 1.0 ]

	str_spcID = string(spcID)
	str_satID = string(satID)
	obsrvr = strtrim(str_spcID)
	target = strtrim(str_satID)

	; fixref的名称格式为:IAU_body name,例如:火卫一,IAU_PHOBOS; 土卫九,IAU_PHOEBE
	cspice_bodc2n, satID, sat_name, found
	if found then begin
	 print, 'sat_name = ', sat_name
	endif
	fixref = 'IAU_' + sat_name
	; cspice_namfrm将参考系名称转换为对应的参考系ID
	cspice_namfrm, fixref, fixref_id
	print, 'fixref = ', fixref

	abcorr = 'CN+S'
	corloc = 'CENTER'
	schstp = 1.0d-4	    ; 搜索角度步长,100微弧
	soltol = 1.0d-7	    ; 收敛角度,100纳弧
	ncuts  = n_points		; 切割半平面的个数
	delrol = cspice_twopi() / ncuts
	; 获得目标星体DSK模型的边缘点坐标，边缘点坐标基于Body-fixed参考系
	cspice_limbpt, method, $
				target, et,     fixref,            $
				abcorr,    corloc, obsrvr, z,      $
				delrol,    ncuts,  schstp, soltol, $
				MAXN, npts, points, epochs, tangts
 
  ; https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_pxform.html#Examples
	; CSPICE_PXFORM返回将位置向量从Body—fixed参考系转换为J2000参考系的变换矩阵rotate
	cspice_pxform, fixref, 'J2000', et-ltime, rotate
	rotate = transpose(rotate)
  
  pvec = dblarr(3, MAXN)
  FOR i = 0, n_elements(tangts[0,*])-1 DO pvec[*,i] = rotate#tangts[*,i]
  print, 'pvec = '
  print, pvec
  
	; 将DSK模型边缘点（由航天器指向边缘点的切向量定义）从J2000(x,y,z)坐标转换为J2000(Ra,Dec)坐标
	cspice_recrad, pvec, range, ra, dec
  
	; 将DSK模型边缘点J2000 Ra/Dec坐标转换为sample/line坐标
	radec2slcoord, ra, dec, sample, line

	; 返回DSK模型边缘点的ra/dec坐标（毫弧）和sample/line坐标
	RETURN, {RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_satModel_spice, @孙德淦，2022.10.11
; PURPOSE: 使用目标星体的椭球模型,寻找从航天器看到的边缘点,返回边缘点的ra/dec坐标和sample/line坐标
; INPUT: 
;     - satID：卫星id，表示要加载哪颗卫星的模型
;     - et： 图像曝光时间中间点
;     - spcID：航天器ID
;     - n_points：模型边缘点数量
;     - CENTER： 模型中心
; OUTPUT:
;     - model：目标体边缘结构体,包含目标体的中心坐标、边缘坐标、明暗线坐标、赤道线坐标
;---------------------------------------------------------------------------------------------------
FUNCTION caviar_satModel_spice, satID, et, spcID, n_points, CENTER=center
  print
  print, '--------------------------caviar_satModel_spice----------------------------'
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
    rho_center=-rho 
  ENDIF

  ; 获得太阳指向目标星体的位置向量(J2000)
  cspice_spkez, 10L, et-ltime, 'J2000', 'CN', satID, state, ltime_sun
  rho_sun=-state[0:2] ; 目标星体指向太阳的位置向量取反,获得太阳指向目标星体的位置向量
  
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

  ; 获取位于x轴/y轴/z轴上的椭圆体半轴的长度
  ; cspice_bodvar返回一个双精度向量,包含存储在内核池中的一些指定主体数据的值。
  ; Input:
  ; 	- satID:目标主体的ID
  ; 	- 'RADII':指定要返回的属性
  ; Output:
  ; - radii:目标主体的半径,双精度3维数组
  cspice_bodvar, satID, 'RADII', radii

  ; caviar_satModel_spice_getPoints返回边缘线/过渡线/赤道线的ra/dec和x/y位置
  ; INPUT:
  ;		- radii:椭圆体三轴半径
  ;		- tipm:将位置向量从惯性参考系转换到body-fixed参考系的变换矩阵
  ;		- n_points:边缘点数量
  ;		- rho:从目标星体指向观察者的位置向量(J2000)
  ;		- rhoi:将rho位置向量从J2000转为body-fixed参考系，同时将列向量转为行向量
  ; OUTPUT:{RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
  ; TRANSPOSE对矩阵或数组进行转置,这里将列向量转为行向量,因为cspice_edlimb要求传入的是行向量(三维数组)
  limb_test = caviar_satModel_spice_ell_getLimbPoints(spcID, satID, 5, et, ltime)
  limb = caviar_satModel_spice_getPoints(radii, tipm, 5, rho, TRANSPOSE(tipm##rho))
  term = caviar_satModel_spice_getPoints(radii, tipm, 5, rho, TRANSPOSE(tipm##rho_sun))
  equa = caviar_satModel_spice_getPoints(radii, tipm, 5, rho, [0.0D, 0.0D, 300000.0D])

  ;# Compute limb center ra/dec & x/y position from rectangular coordinates (rho_center)
  ; 计算J2000三维直角坐标下物体中心的ra/dec和x/y位置（rho_center）
  cspice_recrad, rho_center, range, rac, decc
  radec2slcoord, rac, decc, sc, lc

  model = {npts: n_points, slCenter: [[sc],[lc]], rdCenter: [[rac],[decc]], $
    slLimb:[[limb.sample], [limb.line]], rdLimb:[[limb.ra], [limb.dec]], $
    slTerm:[[term.sample], [term.line]], rdTerm:[[term.ra], [term.dec]], $
    slEqua:[[equa.sample], [equa.line]], rdEqua:[[equa.ra], [equa.dec]] $
  }

  RETURN, model
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_satModel_spice_dsk_getLimbPoints, @孙德淦，2022.10.10
; PURPOSE: 使用目标星体的DSK模型寻找从航天器看到的边缘点,返回边缘点的ra/dec坐标和sample/line坐标
; INPUT:
;     - satID：目标星体ID
;     - spcID：航天器ID
;     - n_points：模型边缘点数量
;     - et： 星历表时间,表示航天器观测星体的时间
;     - ltime:目标星体和航天器之间的单向光时间
; OUTPUT:返回目标星体DSK模型边缘点的ra/dec坐标（毫弧）和sample/line坐标
;     - {RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
;---------------------------------------------------------------------------------------------------
FUNCTION caviar_satModel_spice_dsk_getLimbPoints, spcID, satID, n_points, et, ltime
  print
  print, '-----------------caviar_satModel_spice_dsk_getLimbPoints--------------------'
  
	; 设置cspice_limbpt函数的参数
	MAXN = n_points	; 可以存储的最大边缘点个数
	method = 'TANGENT/DSK/UNPRIORITIZED'
	z = [ 0.D, 0.0, 1.0 ]

	str_spcID = string(spcID)
	str_satID = string(satID)
	obsrvr = strtrim(str_spcID)
	target = strtrim(str_satID)

	; fixref的名称格式为:IAU_body name,例如:火卫一,IAU_PHOBOS; 土卫九,IAU_PHOEBE
	cspice_bodc2n, satID, sat_name, found
	if found then begin
	 print, 'sat_name = ', sat_name
	endif
	fixref = 'IAU_' + sat_name
	; cspice_namfrm将参考系名称转换为对应的参考系ID
	cspice_namfrm, fixref, fixref_id
	print, 'fixref = ', fixref

	abcorr = 'CN+S'
	corloc = 'CENTER'
	schstp = 1.0d-4	    ; 搜索角度步长,100微弧
	soltol = 1.0d-7	    ; 收敛角度,100纳弧
	ncuts  = n_points		; 切割半平面的个数
	delrol = cspice_twopi() / ncuts
	; 获得目标星体DSK模型的边缘点坐标，边缘点坐标基于Body-fixed参考系
	cspice_limbpt, method, $
				target, et,     fixref,            $
				abcorr,    corloc, obsrvr, z,      $
				delrol,    ncuts,  schstp, soltol, $
				MAXN, npts, points, epochs, tangts
 
  ; https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_pxform.html#Examples
	; CSPICE_PXFORM返回将位置向量从Body—fixed参考系转换为J2000参考系的变换矩阵rotate
	cspice_pxform, fixref, 'J2000', et-ltime, rotate
	rotate = transpose(rotate)
  
  pvec = dblarr(3, MAXN)
  FOR i = 0, n_elements(tangts[0,*])-1 DO pvec[*,i] = rotate#tangts[*,i]
  
	; 将DSK模型边缘点（由航天器指向边缘点的切向量定义）从J2000(x,y,z)坐标转换为J2000(Ra,Dec)坐标
	cspice_recrad, pvec, range, ra, dec
  
	; 将DSK模型边缘点J2000 Ra/Dec坐标转换为sample/line坐标
	radec2slcoord, ra, dec, sample, line

	; 返回DSK模型边缘点的ra/dec坐标（毫弧）和sample/line坐标
	RETURN, {RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_satModel_spice_dsk_getTermPoints, @孙德淦，2022.10.11
; PURPOSE: 使用目标星体的DSK模型寻找从航天器看到的terminator边缘点,返回terminator边缘点的ra/dec坐标和sample/line坐标
; INPUT:
;     - satID：目标星体ID
;     - spcID：航天器ID
;     - n_points：模型边缘点数量
;     - et： 星历表时间,表示航天器观测星体的时间
;     - ltime:目标星体和航天器之间的单向光时间
; OUTPUT:返回目标星体DSK模型terminator边缘点的ra/dec坐标（毫弧）和sample/line坐标
;     - {RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
;---------------------------------------------------------------------------------------------------
FUNCTION caviar_satModel_spice_dsk_getTermPoints, spcID, satID, n_points, et, ltime
  print
  print, '-----------------caviar_satModel_spice_dsk_getTermPoints--------------------'
  
	; 设置cspice_termpt函数的参数
	MAXN = n_points	; 可以存储的最大边缘点个数
	method = 'PENUMBRAL/TANGENT/DSK/UNPRIORITIZED'	; terminator定义为半影
	z = [ 0.D, 0.0, 1.0 ]
	ilusrc = 'SUN'	; 光源
	
	str_spcID = string(spcID)
	str_satID = string(satID)
	obsrvr = strtrim(str_spcID)
	target = strtrim(str_satID)

	; fixref的名称格式为:IAU_body name,例如:火卫一,IAU_PHOBOS; 土卫九,IAU_PHOEBE
	cspice_bodc2n, satID, sat_name, found
	if found then begin
	 print, 'sat_name = ', sat_name
	endif
	fixref = 'IAU_' + sat_name
	; cspice_namfrm将参考系名称转换为对应的参考系ID
	cspice_namfrm, fixref, fixref_id
	print, 'fixref = ', fixref

	abcorr = 'CN+S'
	corloc = 'CENTER'
  schstp = 1.0d-4     ; 搜索角度步长,100微弧
  soltol = 1.0d-7     ; 收敛角度,100纳弧
	ncuts  = n_points; 切割半平面的个数
	delrol = cspice_twopi() / ncuts
	; 获得目标星体DSK模型的terminator坐标,坐标基于Body-fixed参考系
	cspice_termpt, method, ilusrc, target, et, fixref, $
				   abcorr, corloc, obsrvr, z,      	   $
				   delrol, ncuts, schstp, 			   $
				   soltol, MAXN, npts, points, epochs, tangts
 
  ; https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_pxform.html#Examples
	; CSPICE_PXFORM返回将位置向量从Body—fixed参考系转换为J2000参考系的变换矩阵rotate
	cspice_pxform, fixref, 'J2000', et-ltime, rotate
	rotate = transpose(rotate)

	; 使用变换矩阵rotate将terminator切向量从Body-fixed (x,y,z)坐标转换成J2000 (x,y,z)坐标
	term_pvec = dblarr(3, MAXN)  ; J2000参考系下的切向量位置,从航天器指向目标星体的terminator
	FOR i = 0, n_elements(tangts[0,*])-1 DO term_pvec[*,i] = rotate#tangts[*,i]
	
	; 将DSK模型terminator边缘点（由航天器指向terminaotor边缘点的切向量定义）从J2000(x,y,z)坐标转换为J2000(Ra,Dec)坐标
	cspice_recrad, term_pvec, range, ra, dec
  
	; 将DSK模型terminaotor边缘点J2000 Ra/Dec坐标转换为sample/line坐标
	radec2slcoord, ra, dec, sample, line

	; 返回DSK模型terminaotor边缘点的ra/dec坐标（毫弧）和sample/line坐标
	RETURN, {RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_satModel_spice_dsk, @孙德淦，2022.10.2
; PURPOSE: 使用目标星体的DSK模型寻找从航天器看到的边缘点,返回边缘点的ra/dec坐标和sample/line坐标
; INPUT:
;     - satID：目标星体ID
;     - et： 星历表时间,表示航天器观测星体的时间
;     - spcID：航天器ID
;     - n_points：模型边缘点数量
;     - CENTER： 图像中指向校正后星体的中心
; OUTPUT:
;     - model：目标体边缘结构体,包含目标体的中心坐标、边缘坐标、明暗线坐标、赤道线坐标
;---------------------------------------------------------------------------------------------------
FUNCTION caviar_satModel_spice_dsk, satID, et, spcID, n_points, CENTER=center
	print
	print, '------------------------caviar_satModel_spice_dsk-------------------------------'

	IF n_points LE 0 THEN MESSAGE, "Variable 'NPTS' must be greater than zero"

  ; 获得目标星体指向航天器的位置向量(J2000)
  ; CSPICE_SPKEZ返回目标体相对于观察者的状态（位置和速度）
  ; https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_spkez.html
  ; INPUT：
  ;   - satID：目标星体ID,目标星体和观察者之间的位置向量从观察者指向目标星体
  ;   - et:星历表时间,表示为J2000TDB之后的秒数,在此时间计算目标体相对于观测者的状态,'et'表示观测者所在位置的时间
  ;   - 'J2000':参考框架的名称,输出的状态矢量应相对于该参考框架表示
  ;   - 'CN+S':一个标量字符串,表示应用于目标体状态的像差校正方式,以考虑到单向光照时间和恒星像差
  ;   - spcID:观测主体的ID
  ; OUTPUT:
  ;   - state: 6维向量,表示目标物体相对于指定观察者的位置（以千米为单位）和以千米/秒为单位的速度
  ;     （state的前三个分量代表目标体位置的x、y和z分量;后三个分量构成相应的速度向量）
  ;   - ltime:观察者和目标之间的单向光照时间,单位为秒
  cspice_spkez, satID, et, 'J2000', 'CN+S', spcID, state, ltime
  rho=-state[0:2] ; 观察者相对于目标星体的位置向量(J2000)
  rho_center=rho  ; rho_center即为从观察者相对于目标体的位置向量(J2000)

  ; KEYWORD_SET函数根据指定表达式的值返回一个字节值,如果其参数已定义且非零，则返回1（真），否则返回0（假）
  ; center是CAVIAR_DATA公共块image结构体中的一个成员变量
	IF KEYWORD_SET(center) THEN BEGIN
		; slcoord2radec是caviar自定义过程,用于将图像的sample/line坐标转换为Ra/Dec坐标
		; INPUT:含有像素的sample/line坐标的标量或数组
		; 可选输入参数:/ITERATE
		; OUTPUT:包含Ra/Dec坐标（单位:弧度）的标量或数组
		slcoord2radec, center[0], center[1], RA, dec, /ITERATE	; 将图像星体的中心sample/line坐标转换为Ra/Dec坐标

		; NORM函数计算向量或二维数组的范数,默认NORM对向量计算二范数,对数组计算0范数
		rho = - NORM(rho) * [cos(dec)*cos(RA), cos(dec)*sin(RA), sin(dec)]	
		rho_center=-rho	
	ENDIF
	
	; 获得太阳指向目标星体的位置向量(J2000)
	cspice_spkez, 10L, et-ltime, 'J2000', 'CN', satID, state, ltime_sun
	rho_sun=-state[0:2] ; 目标星体指向太阳的位置向量取反,获得太阳指向目标星体的位置向量

	; 将图像目标体中心的J2000(x,y,z)坐标转换为J2000 RA/DEC坐标
	cspice_recrad, rho_center, range, rac, decc

	; 将J2000 RA/DEC坐标转换为；sample/line坐标
	radec2slcoord, rac, decc, sc, lc
	
	; 调用相关函数返回对应的边缘点
	limb = caviar_satModel_spice_dsk_getLimbPoints(spcID, satID, n_points, et, ltime)
	term = caviar_satModel_spice_dsk_getTermPoints(spcID, satID, n_points, et, ltime)
  ;equa = caviar_satModel_spice_dsk_getEquaPoints(spcID, satID, n_points, et, ltime)
	
	model = {npts: n_points, slCenter: [[sc],[lc]], rdCenter: [[rac],[decc]], $
    slLimb:[[limb.sample], [limb.line]], rdLimb:[[limb.ra], [limb.dec]], $
    slTerm:[[term.sample], [term.line]], rdTerm:[[term.ra], [term.dec]], $
    slEqua:[[0], [0]], rdEqua:[[0], [0]] $
  }
  
	RETURN, model
END