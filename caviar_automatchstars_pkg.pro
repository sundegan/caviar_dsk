;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; List of procedures/functions of this file:
;	function GetFileNames
;	function NumStarsOutRange
;	function caviar_getTargetRect
;	pro      FindImgStars
;	pro      caviar_imgstars_clear
;	pro      Write2Log
;	
;	
;	the pro file is created by zhangqf 2018, Feb
;-------------------------------------------------------------------------------


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;   GetFileNames
; PURPOSE:
;   get all IMG files' names from the specific directory.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;   GetFileNames(type)
;   
; INPUTS:
;   type:  =1,  get IMG files' names from the default caviar image directory.
; 
; OUTPUTS:
;   filenames: list of all files' names.
;
; KEYWORDS:
; 
;
; COMMON BLOCKS:
;   None.
; SIDE EFFECTS:
;   None.
;
; MODIFICATION HISTORY:
; 2018, Feb   Qingfeng Zhang    JNU
;   
; 2018, Mar   Qingfeng Zhang    JNU   
;     delete the "type=2", only read filename list from default direction.
; 
;-------------------------------------------------------------------------------
FUNCTION GetFileNames,type

    images_path = GETENV('CAVIAR_IMAGES_DIRECTORY')
    ;    if images_path.endswith(path_sep()) then images_path=images_path+'test2'  $
    ;    else  images_path=images_path+'/test2'
    ;    print, images_path
    FileNames=file_search(images_path, '*.IMG', COUNT=cn)
    return, FileNames

END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;   NumStarsOutRange
; PURPOSE:
;   Count the number of stars out of specific rect in one image.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;   NumStarsOutRange(rect, s)
;
; INPUTS:
;    inRect: the rect excluded in one image when counting number of stars
;    s:      list of detected stars in an image, including their positions.
;            its structure is same as the fnd in another pro file.
;    
; OUTPUTS:
;   the number of stars out of range in one image.
;
; KEYWORDS:
;
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;   None.
;
; MODIFICATION HISTORY:
; 2018, Feb   Qingfeng Zhang    JNU
;   - Written
;
;-------------------------------------------------------------------------------
FUNCTION NumStarsOutRange, inRect, s

  j=0
  nn=n_elements(s.x)
 
  FOR i=0, nn-1  DO BEGIN
    if s.x[i] LT inRect.LEFT   then j++  &  continue
    if s.x[i] GT inRect.RIGHT  then j++  &  continue
    if s.y[i] LT inRect.BOTTOM then j++  &  continue
    if s.y[i] GT inRect.TOP    then j++  &  continue
  END  
  return, j
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;   getIndexStarsInRect
; PURPOSE:
;   give the falg array to indicate if the star is in one rect.
;   
; CATEGORY:
;
; CALLING SEQUENCE:
;   getIndexStarsInRect, inrect , s
;
; INPUTS:
;    inRect: the rect excluded in one image
;    s:      list of structure of detected stars in an image, including their positions.
;            its structure is same as the imgstars.
;   
; OUTPUTS:
;     inout:  the index array indicate if the stars[i] in the specific rect.
;     inout[i]=1  means the ith star is out of the rect.
;     inout[i]=0  means the ith star is in the rect.
; KEYWORDS:
;
;
; COMMON BLOCKS:
;    NONE
; SIDE EFFECTS:
;    None.
;
; MODIFICATION HISTORY:
; 2018, Feb   Qingfeng Zhang    JNU
;   - Written
;
;-------------------------------------------------------------------------------
FUNCTION getIndexStarsInRect, inrect , s
  nn=n_elements(s)
  inout=MAKE_ARRAY(nn, /INTEGER, VALUE = 0)
  
  FOR i=0, nn-1  DO BEGIN
    ele=s[i]
    x=ele.xcent
    y=ele.ycent
    if ( (x LT inRect.LEFT) || (x GT inRect.RIGHT) || (y LT inRect.BOTTOM) ||  (y GT inRect.TOP ) ) then begin
      inout[i]=1
      continue
    endif
  ENDFOR
  
  return, inout
END 


FUNCTION isInRect, x, y, inRect
  rtn= !true
  if ( (x LT inRect.LEFT) || (x GT inRect.RIGHT) || (y LT inRect.BOTTOM) ||  (y GT inRect.TOP ) ) then begin
    rtn= !false
  endif
  return, rtn
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;   caviar_getTargetRect
; PURPOSE:
;   get the rect including the target object.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;   target: the name of target object.
;   ityep:   =1,  for couting image stars in a bigger rect.
;            =2,  for removing image stars in the strict small rect.
;            =3,  for removing image stars in one smaller rect.
;
; OUTPUTS:
;   rect: the rect of the taget object in one image.
;         rect={LEFT:smin, RIGHT:smax, TOP:lmax, BOTTOM:lmin}
;
; KEYWORDS:
;
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;   None.
;
; MODIFICATION HISTORY:
; 2018, Feb   Qingfeng Zhang    JNU
;   - Written
;
;-------------------------------------------------------------------------------
FUNCTION caviar_getTargetRect, target, itype

  COMMON CAVIAR_DATA, image, catstars, imgstars, planets, rings
  
  index=-1
  FOREACH ele, planets, index DO BEGIN
    if strupcase(ele.name)  EQ strupcase(target) then  begin
      sat = planets[index] 
      break
    endif
  ENDFOREACH
 
  if index EQ -1  then begin
    msg="please load satellite: " + target
    message, msg 
    return, !NULL
  endif
  
  
  ; Get size, in pixels, of the radius of the satellite on the sensor:
  cspice_spkez, sat.ID, image.ET, 'J2000', 'CN+S', image.SPC.ID, satState, ltime
  satPixRadius = ATAN(MAX(sat.RADII), NORM(satState[0:2])) / (image.FOVPIX*image.BINNING)

  ; for couting image stars in a bigger rect.
  if itype EQ 1 then   hbox = satPixRadius + (3 > 0.05*satPixRadius < 7)
  ; for removing image stars in the strict little rect.
  if itype EQ 2 then   hbox = satPixRadius
  ;if itype EQ 3 then   hbox = (2 > (satPixRadius-30) ) 
  
  
  smin = ( ROUND( sat.xcoord - hbox ) > 0 )[0]
  smax = ( ROUND( sat.xcoord + hbox ) < (image.ns-1) )[0]
  lmin = ( ROUND( sat.ycoord - hbox ) > 0 )[0]
  lmax = ( ROUND( sat.ycoord + hbox ) < (image.nl-1) )[0]
  rect1 ={LEFT:smin, RIGHT:smax, BOTTOM:lmin, TOP:lmax}
  
  return, rect1
end


;;; here, assume that there are duplicate element in only array1, 
;;  each element in array2 is unique.
;function getDupElement,arr1, arr2
;  count=0
;  n=n_elements(arr1)
;  for i= 0, n-1 do begin
;    elemi=arr1[i]
;    count=0
;    for j=i, n-1 then begin
;      if elemi EQ arr1[j] then begin
;        dupelem=elemi
;        count++
;      endif
;      
;    endfor
;    
;  endfor
;
;end

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;   write2log
; PURPOSE:
;   output messages to log file.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;   write2log( filename, msg)
;
; INPUTS:
;    Filename:   the name of processing file
;    msg:        the message that will be write into the log file
;
; OUTPUTS:
;   NONE
;
; KEYWORDS:
;
;
; COMMON BLOCKS:
;   None.
; SIDE EFFECTS:
;   None.
;
; MODIFICATION HISTORY:
; 2018, Feb   Qingfeng Zhang    JNU
;   - Written
;
;-------------------------------------------------------------------------------
pro Write2Log, filename, msg

  ;logfilename="/usr/data/Iapetus/result.log"
  logfilename = FILE_DIRNAME(filename)+PATH_SEP()+"log.txt"

  ;  case flag of
  ;    0:  msg="Warning! Target name is not Iapetus."
  ;    1:  msg="Failure! Cann't get planets information enough. "
  ;    2:  msg="Failure! The number of matching stars is less than 3."
  ;    21: msg="Failure! The number of matching stars is less than 2."
  ;    3:  msg="Failure! Limb fitting cann't pass."
  ;    4:  msg="Success! All step passed."
  ;    10: msg="Failure! Cann't get header information from header file."
  ;    11: msg="Failure! Cann't get parameters from header file."
  ;    12: msg="Failure! Cannot found camera pointing matrix."
  ;    13: msg="Failure! Cannot detect edges with setting parameters."
  ;    14: msg="Failure! Observating object Iapetus is not in the image."
  ;  endcase

  b=bin_date(systime())
  date1=string(b[0],b[1],b[2],b[3],b[4],b[5], format='(i-5, i02," ",i02," ", i02,":",i02,":",i02   )')
  openw, lun, logfilename, /GET_LUN,/APPEND
  printf, lun, filename, date1, MSG, format='(A-70,A-22, A-80)'
  close,lun
  free_lun, lun
end



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;   FindImgStars
; PURPOSE:
;   detect image stars with proper parameter, and remove the stars in the reange covered by the target.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;   FindImgStars(target [ , UPNUMSTARS=upnumstars])
;
; INPUTS:
;    target:   the object will be exclueded when counting the number of image stars.
;   
; OUTPUTS:
;   NONE
;
; KEYWORDS:
;    UPNUMSTARS=upnumstars, the up threshold of number of image stars.
;                 default is 600.
;
; COMMON BLOCKS:
;   COMMON CAVIAR_DATA, image, catstars, imgstars, planets, rings
;   COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
;   
; SIDE EFFECTS:
;   None.
;
; MODIFICATION HISTORY:
; 2018, Feb   Qingfeng Zhang    JNU
;   - Written
;
;-------------------------------------------------------------------------------
PRO FindImgStars, target

  COMMON CAVIAR_DATA, image, catstars, imgstars, planets, rings
  COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
  

  params = find_imgStars_params
  fnd = caviar_imgstars_find(image.RAW, params.FWHM, params.NSIG, $
    params.ROUNDLIM, params.SHARPLIM, CENTROID_METHOD=2, REMOVE_SATURATED=1, STATISTIC=stat)

  rect1=caviar_getTargetRect(target, 1)
  rect2 ={LEFT:6, RIGHT:image.ns-6, BOTTOM:6 , TOP:(image.nl-6) }
  
  nn=n_elements(fnd.x)
  
  imgstars = LIST()
  
  FOR i=0, nn-1 DO BEGIN
      a= isInRect(fnd.X[i], fnd.Y[i], rect1 ) 
      b= isInRect(fnd.X[i], fnd.Y[i], rect2 ) 

      if ((not a) && b ) then begin
        star = {XCENT:fnd.X[i], YCENT:fnd.Y[i], FLUX:fnd.FLUX[i], $
          SHARPNESS:fnd.SHARPNESS[i], ROUNDNESS:fnd.ROUNDNESS[i]}
        imgstars.add, star, /EXTRACT
      endif 
  ENDFOR

END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;   caviar_imgstars_clear
; PURPOSE:
;   remove the detected image stars on the surface of target object from list of imgstars.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;   
;
; INPUTS:
;    target: the specified target that will be excluded.
; OUTPUTS:
;   NONE
;
; KEYWORDS:
;
;
; COMMON BLOCKS:
;    
; SIDE EFFECTS:
;   None.
;
; MODIFICATION HISTORY:
; 2018, Feb   Qingfeng Zhang    JNU
;   - Written
;
;-------------------------------------------------------------------------------
PRO caviar_imgstars_clear, target

    COMMON CAVIAR_DATA, image, catstars, imgstars, planets, rings
  
    rect=caviar_getTargetRect(target, 2)
    if rect EQ !NULL then return
    
    smin = rect.left
    smax = rect.right
    lmin = rect.bottom
    lmax = rect.top

    k=0
    for i=0, n_elements(imgstars)-1 do begin
      x=imgstars[i].xcent
      y=imgstars[i].ycent
      if ((x GT smin) && (x LT smax) && (y GT lmin) && (y LT lmax)) then  begin
        if (k EQ 0) then ind=i else ind=[ind, i]
        k++
      endif
    endfor
    if ~(ind EQ !NULL) then imgstars.remove, ind

  
END