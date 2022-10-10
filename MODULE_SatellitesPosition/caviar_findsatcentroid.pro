;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_FINDSATCENTROID_COLOFFSET
; PURPOSE: Compute the object's center of light offset to correct for the phase 
;		angle and obtain the center of figure.
; INPUTS:
;	pid: Scalar integer. NAIF integer code of the planet (target body). Ex: 501=Phobos
;
; OUTPUT:
;	offset: 2-Elements array of double. Sample and line offset between the 
;		Center Of Light and the Center Of Figure (in pixel) due to phase  
;		angle between spacecraft, planet (or satellite) and sun.
;-------------------------------------------------------------------------------

FUNCTION caviar_findsatcentroid_coloffset, pid, et, imgpixfov, spcID, cmat
	
;	CATCH, error_status
;	IF error_status NE 0 THEN BEGIN
;		CATCH, /CANCEL
;		pos = STRPOS(!ERROR_STATE.MSG, ']')
;		PRINT, !ERROR_STATE.MSG_PREFIX+STRMID(!ERROR_STATE.MSG, 0, pos+1)
;		s = STRSPLIT(STRMID(!ERROR_STATE.MSG, pos+2), '(  +)', /REGEX, /EXTRACT)
;		s[0] = STRMID(s[0], 0, STRLEN(s[0])-1) 
;		PRINT, STRJOIN(s, ' ')
;		MESSAGE, "Cannot compute COF/COL offset of body "+STRING(pid,'(I3)')
;		RETURN, [0D,0D]
;	ENDIF
		
	; Get position vector of:
	; - the spacecraft (observing body) relative to the planet (target): rho1
	; - the planet (observing body) relative to the sun (target): rho2
	cspice_spkezp, pid, et, 'J2000', 'CN+S', spcID, rho1, ltime
	cspice_spkezp, 10L, et-ltime, 'J2000', 'CN+S', pid, rho2, ltime
	
	; Get seperation angle between spacecraft and sun, viewed from the satellite:
	phi = cspice_vsep(-rho1,rho2)
	
	; Get maximum radius of the satellite on the sensor:
	cspice_bodvar, pid, 'RADII', radii
	radpix = ATAN( MAX(radii),NORM(-rho1) ) / imgpixfov
	
	rho_camframe = cmat ## rho2
	rho_camframe /= NORM(rho_camframe)
	
	corr = -radpix * (3*!DPI*SIN(phi)*(1+COS(phi))) / (16*(SIN(phi)+((!DPI-phi)*COS(phi))))

	offset = rho_camframe * corr
	
	RETURN, offset
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_CTRD_MM_SAT
; PURPOSE: compute the center position of satellite by modified method.
; added by zhangqf at JNU
;
; 注意，这里的x0.y0是前面经过了理论值，找峰值，然后cntrd准确计算的过程，所以
; 这里输入的x0.y0应该是比较准确的，这里再一次找峰值，应该可以在1个像素半径内，
; 为扩大鲁棒性，扩展为2个像素半径内寻找，考虑到环缝中的因素，不要太大，
; 太大可能被其它像素干扰，找到假中心。
;-------------------------------------------------------------------------------
FUNCTION CAVIAR_CTRD_MM_SAT, InIMG,x0,y0, fwhm
  centxy=dblarr(2,1)

  centxy(0)=x0
  centxy(1)=y0
  nxy = Size(inIMG, /Dimensions)

  ;中心在边缘6个像素范围内，则不测量，直接保持原值,返回。
  if  ( (x0 LT 6) OR (x0 GT nxy[0]-7) OR (y0 LT 6) OR (y0 GT nxy[1]-7) ) then return, centxy

  x=round(x0)
  y=round(y0)
  ;find where is the peak. In general, it is (x,y).
  ;Sometime, it is the neighbour pixel.  
  r1=2
  subImg= InIMG[x-r1:x+r1, y-r1:y+r1]
  peak=max(subImg, indx)
  xmax=x-r1+ (indx mod (2*r1+1))
  ymax=y-r1+ (indx / (2*r1+1) )

  ;xmax=350
  ;ymax=654
  ;fwhm<2 means undersample.  according to different fwhm, the inner radius and outer radius are
  ; taken different values. 
  if (fwhm LE 1.3) then begin
    inR=1
    outR=3
  endif
  if (fwhm GT 1.3) AND (fwhm LE 2.5) then begin
    inR=2
    outR=4
  endif
  if (fwhm GT 2.5) AND (fwhm LE 4.0) then begin
    inR=3
    outR=5
  endif
  if (fwhm GT 4.0) AND (fwhm LE 6.0) then begin
    inR=4
    outR=7
  endif
  if (fwhm GT 6.0) AND (fwhm LE 7.0) then begin
    inR=5
    outR=8
  endif
  if (fwhm GT 7.0) AND (fwhm LE 8.0) then begin
    inR=6
    outR=9
  endif
  if (fwhm GT 8.0) AND (fwhm LE 9.5)  then begin
    inR=7
    outR=10
  endif
  if (fwhm GT 9.5)  then begin
    inR=8
    outR=11
  endif
  print, fwhm
  print, inr, outr
;  xmax=516
;  ymax=508
;  inR=3
;  outR=6
  

  ;避开边缘，如果是背景就在边缘（3个像素内），那么不处理。
  if ( xmax-outR GE 4 ) AND ((xmax+outR) LT nxy[0]-3) AND ((ymax-outR) GE 4) AND ((ymax+outR) LT nxy[1]-3) then begin
    print, "Initial center:"
    print, x0,y0
    print, "Nearest Peak:"
    print, xmax, ymax
    print, "Whole sub image"
    print, InIMG[xmax-outR:xmax+outR, ymax-outR:ymax+outR]
    bgGray= getBackGround(inImg, xmax, ymax, inR, outR)
    ;Get the ROI that is inR X inR,这里设置为5X5区域，相当于3-4fwhm做内径
    subImg1= InIMG[xmax-inR:xmax+inR, ymax-inR:ymax+inR]
    print, "Background Threshold"
    print, bgGray
    print, "Foreground sub image:"
    print, subimg1
    box1=double(subImg1)-bgGray
    box1[WHERE(box1 LT 0, /NULL)] = 0.0D
    totalMass = Total(box1)
    ;如果totalmass=0, 意味着所有的前景区域的值都比门槛值低。
    ;本质上就是背景取出来的门槛值太高了。这很可能是背景比较杂乱，而前景像素灰度很低。
    ;实际上find发现的星体是导星法，非常敏感。肉眼和数据看起来不是星的也会给归纳为星体。
    ;实际上最好将这一类星体当做非星体，取消。
    ;不过这里为了减少改动，暂时维持，不用修正矩来计算。
    if totalMass EQ 0.0 then begin
      message, "The satellite cannot be measured by modified moment, so keep the centoid by CTRD", /continue
      return, centxy
    endif else  begin
      ;compute controid by modified moment
      s = Size(box1, /Dimensions)      
      xcm = Total( Total(box1, 2) * Indgen(s[0]) ) / totalMass
      ycm = Total( Total(box1, 1) * Indgen(s[1]) ) / totalMass
      centxy(0)=xmax-inR+xcm
      centxy(1)=ymax-inR+ycm
    endelse

  endif

  return, centxy
end



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_FINDSATCENTROID_MM
; PURPOSE: Compute center of light and phase angle offset for unresolved satellite.
; NOTE: Use Modified Moment method to find center.
; INPUTS:
;	img: 2-Dimensionnal array. Image with the satellite centroid to locate.
;	satID: Satellite ID (NAIF integer code).
;	sat[X/Y]pos: [Scalar Integer/Float/Double] Estimated satellite position in img.
;	satRadiusKm: [Scalar Integer/Float/Double] Satellite radius in kilometers.
;		For body with different x/y/z radii, take the max! 
; KEYWORDS:
;	SILENT: If set to 1, no output will be made, otherwise it displays distance, 
;		    radius and fwhm of the satellite.
; PROCEDURES/FUNCTIONS CALLS:
;	CAVIAR_IMGSTARS_FIND_MM in caviar_imgstars_pkg.pro
;	    
;	April, 2021  created by Zhangqf    
;	   
;-------------------------------------------------------------------------------
FUNCTION caviar_findsatcentroid_MM, img, satID, satRadiusKm, satXpos, satYpos, $
						et, imgpixfov, spcID, cmat,	SILENT=SILENT
				
	imgSize = SIZE(img)
	ns = imgSize[1]
	nl = imgSize[2]
	
	;***************************************************************************
	; Get fwhm of the pointed object:
	;***************************************************************************
	cspice_spkezp, satID, et, 'J2000', 'CN+S', spcID, ptarg, ltime
	distance = NORM(ptarg)
	satRadiusPix = ATAN(satRadiusKm,distance) / imgpixfov
	fwhm = (satRadiusPix GT 0.5) ? 2*satRadiusPix : 1
	
	IF NOT KEYWORD_SET(SILENT) THEN BEGIN
		PRINT, FORMAT='("Distance  (km) = ", F-0.3)', distance
		PRINT, FORMAT='("Radius    (km) = ", F-0.3)', satRadiusKm
		PRINT, FORMAT='("Radius (pixel) = ", F-0.3)', satRadiusPix
		PRINT, FORMAT='("FWHM   (pixel) = ", F-0.3)', fwhm
	ENDIF

	;***************************************************************************	
	; Get sub-image around the satellite estimated position 
	; with size of 3 times the object fwhm:
	; 这里本质上需要调整，可以使用2Fwhm，也可以3-4fwhm，理论值和峰值有时候差的很远，就需要扩大。
	; 不同星体情况有比较大的差别，太大又有可能有干扰像素，所以将来应该有手动调整的参数。
	;***************************************************************************
	xmin = ROUND( satXpos - ROUND(fwhm*3)-1 ) > 0
	ymin = ROUND( satYpos - ROUND(fwhm*3)-1 ) > 0
	xmax = ROUND( satXpos + ROUND(fwhm*3)+1 ) < ns-1
	ymax = ROUND( satYpos + ROUND(fwhm*3)+1 ) < nl-1
	sub_img = img[xmin:xmax,ymin:ymax]
	
	;***************************************************************************	
	; Search object centroid:
	;***************************************************************************

	peak=max(sub_img, indx)
	xPeak= xmin + (indx mod (xmax-xmin+1))
	yPeak= ymin + (indx / (ymax-ymin+1) )
	
	;Using gaussian fitting to get centroid. Here, the function gcntrd is not as gauss2dfit, 
	;it can bring bigger error than Gauss2dfit 
	;gcntrd, img, xPeak, yPeak, xcent, ycent, fwhm
	;-------------------------
	yfit = mpfit2dpeak(sub_img, B, /TILT)
	print, "mpfitedpeak results: "
  print, "B is "
  print, B
  xcent=xmin+B[4]
  ycent=ymin+B[5]
  print, "x,y centers are "
  print, xcent, ycent
  print, 2.35*B[2], 2.35*B[3]
  print, "x,y peak are"
  print, xpeak, ypeak
  print, "fwhm is ", fwhm
  print, "---------------"
  read, ff1

  ;----------------------
	
	
  ;If gcntrd return error, then xcent=-1.0 and/or ycent=-1.0
  ;When the gcntrd doesn't get proper centroid. the center is the position with peak grey.
  if ((xcent EQ -1.0) or (ycent EQ -1.0) ) then begin
    xcent=xPeak
    ycent=yPeak
  endif
	
  ; for the undersampled satellite, correct the position by modified moment.
  ; Here the fwhm from apparent radius, it maybe not good. Hence, set the threshold =2.8
	IF fwhm LT 15.0 THEN BEGIN
	 print, "using modified moment to correct the result from Gaussian 2D fitting."
    xym=CAVIAR_CTRD_MM_SAT(img, xcent, ycent, fwhm)
    xcent=xym[0]
  	ycent=xym[1]
	ENDIF
	
	RETURN, {XC:xcent, YC:ycent}
		
;	;***************************************************************************
;	; Compute phase angle offset:
;	;***************************************************************************
;	offset = caviar_findsatcentroid_coloffset(satID, et, imgpixfov, spcID, cmat)
;	PRINT, "INFORMATION: Output x/y centroid values have been corrected for ", $
;		   "phase angle by the x/y offset values, respectively!"
;	PRINT, ""
;	
;	RETURN, {XCENT:xcent+offset[0], YCENT:ycent+offset[1], SIGMA:0.5D, XOFFSET:offset[0], YOFFSET:offset[1]}
END


;This the original gaussian fitting method in Caviar, 
; original name: caviar_findsatcentroid
;  changed name: caviar_findsatcentroid_GS
; when open the sentence below in caviar.pro, it is called.
;  SETENV, 'CAVIAR_IMGSTAR_CENTROIDING=Gaussian'   
FUNCTION caviar_findsatcentroid_GS, img, satID, satRadiusKm, satXpos, satYpos, $
            et, imgpixfov, spcID, cmat, SILENT=SILENT
            
  imgSize = SIZE(img)
  ns = imgSize[1]
  nl = imgSize[2]

  ;***************************************************************************
  ; Get fwhm of the pointed object:
  ;***************************************************************************
  
  cspice_spkezp, satID, et, 'J2000', 'CN+S', spcID, ptarg, ltime
  distance = NORM(ptarg)
  print, ptarg
  print, imgpixfov
  
  satRadiusPix = ATAN(satRadiusKm,distance) / imgpixfov
  fwhm = (satRadiusPix GT 0.5) ? 2*satRadiusPix : 1.3

  IF NOT KEYWORD_SET(SILENT) THEN BEGIN
    PRINT, FORMAT='("Distance  (km) = ", F-0.3)', distance
    PRINT, FORMAT='("Radius    (km) = ", F-0.3)', satRadiusKm
    PRINT, FORMAT='("Radius (pixel) = ", F-0.3)', satRadiusPix
    PRINT, FORMAT='("FWHM   (pixel) = ", F-0.3)', fwhm
  ENDIF

  ;***************************************************************************
  ; Get sub-image around the satellite estimated position
  ; with size of 2 times the object fwhm:
  ;***************************************************************************
  xmin = ROUND( satXpos - ROUND(fwhm*2)-1 ) > 0
  ymin = ROUND( satYpos - ROUND(fwhm*2)-1 ) > 0
  xmax = ROUND( satXpos + ROUND(fwhm*2)+1 ) < ns-1
  ymax = ROUND( satYpos + ROUND(fwhm*2)+1 ) < nl-1
  sub_img = img[xmin:xmax,ymin:ymax]

  print, "sub_img:"
  print, sub_img
  ;***************************************************************************
  ; Search object centroid:
  ;***************************************************************************
  rslt = caviar_imgstars_find(sub_img, fwhm, 1.0, [-1.0,1.0], [0.2,1.0], $
    CENTROID_METHOD=2, /SILENT)

  IF rslt EQ !NULL THEN MESSAGE, "Cannot find centroid."
  max_flux = MAX(rslt.flux, index)
  IF N_ELEMENTS(index) GT 1 $
    THEN MESSAGE, 'More than one local maximum has been found. All maxima have been returned.'

  xcent = xmin + rslt.X[index]
  ycent = ymin + rslt.Y[index]

  IF xcent LT 0 || xcent GT ns-1 || ycent LT 0 || ycent GT nl-1 $
    THEN MESSAGE, 'Error in centroiding this object. Cannot obtain valid location.'
   
  print, "using gaussian fitting"
  RETURN, {XC:xcent, YC:ycent}
;  ;***************************************************************************
;  ; Compute phase angle offset:
;  ;***************************************************************************
;  offset = caviar_findsatcentroid_coloffset(satID, et, imgpixfov, spcID, cmat)
;  PRINT, "INFORMATION: Output x/y centroid values have been corrected for ", $
;    "phase angle by the x/y offset values, respectively!"
;  PRINT, ""
;
;  RETURN, {XCENT:xcent+offset[0], YCENT:ycent+offset[1], SIGMA:0.5D, XOFFSET:offset[0], YOFFSET:offset[1]}
end

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_FINDSATCENTROID
; PURPOSE: Compute center of light and phase angle offset for unresolved satellite.
; NOTE: Use the modified moment method or the find_stars adapted from 1986
;       STSDAS version of DAOPHOT.
; INPUTS:
; img: 2-Dimensionnal array. Image with the satellite centroid to locate.
; satID: Satellite ID (NAIF integer code).
; sat[X/Y]pos: [Scalar Integer/Float/Double] Estimated satellite position in img.
; satRadiusKm: [Scalar Integer/Float/Double] Satellite radius in kilometers.
;   For body with different x/y/z radii, take the max!
; KEYWORDS:
; SILENT: If set to 1, no output will be made, otherwise it displays distance,
;       radius and fwhm of the satellite.
; PROCEDURES/FUNCTIONS CALLS:
; CAVIAR_IMGSTARS_FIND in caviar_imgstars_pkg.pro
;
; April, 2021  modifed by Zhangqf
;     It has been modified greatly. original source codes see also other version of caviar.
;--------------------------------------------------------------------------------------
FUNCTION caviar_findsatcentroid, img, satID, satRadiusKm, satXpos, satYpos, $
  et, imgpixfov, spcID, cmat,  SILENT=SILENT

  mytemp = GETENV('CAVIAR_SATELLITE_CENTROIDING')
  IF ( STRCMP(mytemp, 'ModifiedMoments',  /FOLD_CASE) ) THEN BEGIN
    xycent= caviar_findsatcentroid_MM( img, satID, satRadiusKm, satXpos, satYpos, et, imgpixfov, spcID, cmat)
  ENDIF

  IF ( STRCMP(mytemp, 'Gaussian',  /FOLD_CASE) ) THEN BEGIN
    xycent=caviar_findsatcentroid_GS(img, satID, satRadiusKm, satXpos, satYpos, et, imgpixfov, spcID, cmat)
  ENDIF

  ;***************************************************************************
  ; Compute phase angle offset:
  ;***************************************************************************
  offset = caviar_findsatcentroid_coloffset(satID, et, imgpixfov, spcID, cmat)
  PRINT, "INFORMATION: Output x/y centroid values have been corrected for ", $
    "phase angle by the x/y offset values, respectively!"
  PRINT, ""

  ;RETURN, {XCENT:xcent+offset[0], YCENT:ycent+offset[1], SIGMA:0.5D, XOFFSET:offset[0], YOFFSET:offset[1]}
  RETURN, {XCENT:(xycent.XC)+offset[0], YCENT: (xycent.YC)+offset[1], SIGMA:0.5D, XOFFSET:offset[0], YOFFSET:offset[1]}

END

;;===============================================
;It is for auto processing
FUNCTION CAVIAR_CTRD_MM_SAT_B, x0, y0, fwhm, xmax, ymax
  COMMON CAVIAR_DATA, image, catstars, imgstars, planets
  COMMON CAVIAR_LOG, logfileName, logFileID, resFileName, resFileID

  centxy=dblarr(2,1)
  centxy(0)=x0
  centxy(1)=y0

  inIMG= image.raw
  ns=image.ns
  nl=image.nl

  ;中心在边缘6个像素范围内，则不测量，直接保持原值,返回。
  if  ( (x0 LT 6) OR (x0 GT ns-7) OR (y0 LT 6) OR (y0 GT nl-7) ) then return, centxy

  ;  x=round(x0)
  ;  y=round(y0)
  ;  ;find where is the peak. In general, it is (x,y).
  ;  ;Sometime, it is the neighbour pixel.
  ;  r1=2
  ;  subImg= InIMG[x-r1:x+r1, y-r1:y+r1]
  ;  peak=max(subImg, indx)
  ;  xmax=x-r1+ (indx mod (2*r1+1))
  ;  ymax=y-r1+ (indx / (2*r1+1) )


  if (fwhm LE 1.3) then begin
    inR=1
    outR=3
  endif
  if (fwhm GT 1.3) AND (fwhm LE 2.5) then begin
    inR=2
    outR=4
  endif
  if (fwhm GT 2.5) AND (fwhm LE 4.0) then begin
    inR=3
    outR=5
  endif
  if (fwhm GT 4.0) AND (fwhm LE 6.0) then begin
    inR=4
    outR=7
  endif
  if (fwhm GT 6.0) AND (fwhm LE 7.0) then begin
    inR=5
    outR=8
  endif
  if (fwhm GT 7.0) AND (fwhm LE 8.0) then begin
    inR=6
    outR=9
  endif
  if (fwhm GT 8.0) AND (fwhm LE 9.5)  then begin
    inR=7
    outR=10
  endif
  if (fwhm GT 9.5)  then begin
    inR=8
    outR=11
  endif

  ;避开边缘，如果是背景就在边缘（3个像素内），那么不处理。
  if ( xmax-outR GE 4 ) AND ((xmax+outR) LT ns-3) AND ((ymax-outR) GE 4) AND ((ymax+outR) LT nl-3) then begin

    printf, logFileID,  "Whole sub image"
    printf, logFileID, InIMG[xmax-outR:xmax+outR, ymax-outR:ymax+outR]
    bgGray= getBackGround(inImg, xmax, ymax, inR, outR)
    ;Get the ROI that is inR X inR,这里设置为5X5区域，相当于3-4fwhm做内径
    subImg1= InIMG[xmax-inR:xmax+inR, ymax-inR:ymax+inR]
    printf, logFileID, "Background Threshold"
    printf, logFileID, bgGray
    printf, logFileID, "Foreground sub image:"
    printf, logFileID, subimg1
    box1=double(subImg1)-bgGray
    box1[WHERE(box1 LT 0, /NULL)] = 0.0D
    totalMass = Total(box1)
    ;如果totalmass=0, 意味着所有的前景区域的值都比门槛值低。
    ;本质上就是背景取出来的门槛值太高了。这很可能是背景比较杂乱，而前景像素灰度很低。
    ;实际上find发现的星体是导星法，非常敏感。肉眼和数据看起来不是星的也会给归纳为星体。
    ;实际上最好将这一类星体当做非星体，取消。
    ;不过这里为了减少改动，暂时维持，不用修正矩来计算。
    if totalMass EQ 0.0 then begin
      message, "The satellite cannot be measured by modified moment, so keep the centoid by CTRD", /continue
      return, centxy
    endif else  begin
      ;compute controid by modified moment
      s = Size(box1, /Dimensions)
      xcm = Total( Total(box1, 2) * Indgen(s[0]) ) / totalMass
      ycm = Total( Total(box1, 1) * Indgen(s[1]) ) / totalMass
      centxy(0)=xmax-inR+xcm
      centxy(1)=ymax-inR+ycm
    endelse

  endif

  return, centxy
end


; the function is for auto processing
FUNCTION caviar_findsatcentroid_MM_B, satID, satRadiusKm, satXpos, satYpos, $
  et, imgpixfov, spcID, cmat

  COMMON CAVIAR_DATA, image, catstars, imgstars, planets
  COMMON CAVIAR_LOG, logfileName, logFileID, resFileName, resFileID
  
  img= image.raw
  ns = image.ns
  nl = image.nl
  
  ;***************************************************************************
  ; Get fwhm of the pointed object:
  ;***************************************************************************
  cspice_spkezp, satID, et, 'J2000', 'CN+S', spcID, ptarg, ltime
  distance = NORM(ptarg)
  satRadiusPix = ATAN(satRadiusKm,distance) / imgpixfov
  fwhm = (satRadiusPix GT 0.5) ? 2*satRadiusPix : 1.3
  
  PRINTF, logFileID, FORMAT='("Distance  (km) = ", F-0.3)', distance
  PRINTF, logFileID, FORMAT='("Radius    (km) = ", F-0.3)', satRadiusKm
  PRINTF, logFileID, FORMAT='("Radius (pixel) = ", F-0.3)', satRadiusPix
  PRINTF, logFileID, FORMAT='("FWHM   (pixel) = ", F-0.3)', fwhm
  
  ;***************************************************************************
  ; Get sub-image around the satellite estimated position
  ; with size of 3 times the object fwhm:
  ; 这里本质上需要调整，可以使用2Fwhm，也可以3-4fwhm，理论值和峰值有时候差的很远，就需要扩大。
  ; 不同星体情况有比较大的差别，太大又有可能有干扰像素，所以将来应该有手动调整的参数。
  ;***************************************************************************
  xmin = ROUND( satXpos - ROUND(fwhm*2)-1 ) > 0
  ymin = ROUND( satYpos - ROUND(fwhm*2)-1 ) > 0
  xmax = ROUND( satXpos + ROUND(fwhm*2)+1 ) < ns-1
  ymax = ROUND( satYpos + ROUND(fwhm*2)+1 ) < nl-1
  sub_img = img[xmin:xmax,ymin:ymax]
  
  ;***************************************************************************
  ; Search object centroid:
  ;***************************************************************************
  
  peak=max(sub_img, indx)
  xPeak= xmin + (indx mod (xmax-xmin+1))
  yPeak= ymin + (indx / (ymax-ymin+1) )
  
  ;Using gaussian fitting to get centroid. Here, the function gcntrd is not as gauss2dfit,
  ;it can bring bigger error than Gauss2dfit
  ;gcntrd, img, xPeak, yPeak, xcent, ycent, fwhm
  ;-------------------------
  yfit = mpfit2dpeak(sub_img, B, /TILT)
  xcent_GS=xmin+B[4]
  ycent_GS=ymin+B[5]
  
  PRINTF, logFileID, "Whole sub_img: "
  PRINTF, logFileID,  sub_img
  PRINTF, logFileID, "mpfit2dpeak results are "
  PRINTF, logFileID,  B
  PRINTF, logFileID, "xcent , ycent by mpfit2dpeak "
  PRINTF, logFileID, xcent_GS, ycent_GS

  ;----------------------
  
  ; for the undersampled satellite, correct the position by modified moment.
  ; Here the fwhm from apparent radius, it maybe not good. Hence, set the threshold =2.8
  IF fwhm LT 15.0 THEN BEGIN    
    xym=CAVIAR_CTRD_MM_SAT_B(xcent_GS, ycent_GS, fwhm, xpeak, ypeak)
       
    xcent_MM=xym[0]
    ycent_MM=xym[1]
    
    printf, logFileID, "modified moment results are； “
    printf, logFileID, xcent_MM, ycent_MM
    
  ENDIF else begin
    printf, logFileID, "fwhm is GE 15, it is not suitable to handle by MM method. xcent=-1.0, ycent=-1.0"
    xcent_MM=-1.0
    ycent_MM=-1.0
  Endelse
  
   RETURN, {XCGS:xcent_GS, YCGS:ycent_GS, XCMM:xcent_MM,YCMM:ycent_MM}

END

