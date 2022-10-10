;print, "Compiling 'caviar_image_pkg.pro' routines and dependencies:"
@headerxtract2
@dialog_getValue
@caviar_loadspicekernels
@caviar_getParams
@caviar_getCmat_pkg
@caviar_data_routinesWrapper

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_image_load_spcId
; PURPOSE: 
;	Search in labels for spacecraft name and compare it with spacecraft list
;	compatible with CaVIaR. If it cannot be found in labels, the name or id of
;	the spacecraft will be asked to the user, either with a dialog or a terminal
;	prompt, depending on whether the 'GUI' keyword is set.
;-------------------------------------------------------------------------------
FUNCTION caviar_image_load_spcId, header, GUI=gui, FOUND=found
	
	cspice_boddef, 'MARINER 9', -9
	
	;***************************************************************************
	; Search spacecraft name in the header:
	;***************************************************************************
	vList = LIST()
	vList.ADD, /EXT, headerXtract(header, '^INSTRUMENT_HOST_NAME$', 'STRING')	  ; for Cassini internal header
	vList.ADD, /EXT, headerXtract(header, 'INSTRUMENT_HOST_NAME', 'STRING', '"', '"') ; for Cassini external vicar header
	
	indices = vList.WHERE(!NULL)
	IF indices NE !NULL THEN vList.REMOVE, indices
	vList = vList.toArray()
	
	spcId = 0
	IF N_ELEMENTS(vList) NE 0 THEN BEGIN
		IF TOTAL(STREGEX(vList, 'CASSINI', /BOOL)) GT 0 		 THEN spcId = -82
	ENDIF
	found = (spcId EQ 0) ? 0 : 1
		
	;***************************************************************************
	; Spacecraft ID is not found, warn the user and ask him which one to choose:
	;***************************************************************************
	WHILE NOT found DO BEGIN
		spcList = ['CASSINI # -82']
		lbl = "Cannot find spacecraft name in the header. Select one from the list:"
		IF KEYWORD_SET(gui) THEN BEGIN
			state = dialog_getValue(LABEL=lbl, VALUE=val, LIST_VALUES=spcList)
			val = STRTRIM((STRSPLIT(val, '#', /EXTRACT))[0], 2)
		ENDIF ELSE BEGIN 
			PRINT, lbl
			FOREACH spc, spcList DO PRINT, '    ', spc
			READ, "Enter the spacecraft ID (eg '-82') or press return to quit: ", (val='')
			state = (val EQ '') ? 0 : 1
			IF NOT STREGEX(val, '-', /BOOL) THEN val = '-'+STRTRIM(val, 2)
		ENDELSE
		IF state EQ 0 THEN RETURN, !NULL
		
		; Get spaceraft NAIF ID from user selection:
		cspice_bods2c, val, spcId, found

		IF NOT found THEN BEGIN
			msg = "Invalid spacecraft name or ID!"
			IF KEYWORD_SET(gui) THEN res = DIALOG_MESSAGE(msg, /CENTER) ELSE PRINT, msg
		ENDIF
	ENDWHILE
	
	RETURN, spcId
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_IMAGE_LOAD
; PURPOSE:
;
; INPUTS:
;	IMGFILE: Scalar string with the path to the image file name.
;	IMGTYPE: Scalar string refering to the image type/format.
;                       Ex: "CASSINI"
; OPTIONNAL INPUT KEYWORDS:
;	LABELS_FILE: Scalar string with the path to the label file name.
;	POINTING_FILE: Scalar string with the path to the file containing the 
;		pointing matrix.
;			  If an empty string is given a dialog will ask the user the file. 
; OUTPUTS: 
;	image = Structure variable with following tags:
;		'PATH' = Full path of the image file.
;		'NAME' = Name of the image file with extension.
;		'WINDOW' = Caviar GUI window where is displayed the image, initialized to -1.
;		'HEADER' = 1 or 2-Column(s) array containing the header of the image.
;		'HDRFRMT' = Scalar string representing the header format. 
;			Either 'INTVIC' or 'EXTVIC'
;		'RAWIMG' = 2-Dimensional float array containing the image. 
;		'BYTEIMG' = 2-Dimensional byte array containing the byte scale image.
;		'NS', 'NL' = Scalar long integer. Number of samples/lines of the image.
;		'ET': Scalar double. Image mid-time in ephemeris seconds past J2000.
;		'EXPOSURE' = Exposure time in seconds.
;		'BINNING' = Scalar integer. Image binning.
;		'TARGET' = Structure variable with following tags:
;			'NAME' = Scalar string. Image target name.
;			'ID' = Scalar long integer. Image target ID.
;		'CAM' = Structure variable with following tags:
;			'LNAME' = Camera long name with spacecraft + camera short name.
;			'ID' = Camera NAIF integer code.
;		'SPC' = Structure variable with following tags:
;			'NAME' = Spacecraft name.
;			'ID' = Spacecraft NAIF integer code.
;		'METAKERNEL' = Scalar string. Spacecraft's SPICE metakernel file full path.
;		'FOVPIX' = Scalar double. Field of view of one pixel of the camera (radian).
;		'FOVIMG' = Scalar double. Field of view of the image in the diagonals (degree).
;		'FOCAL' = Scalar double. Focal length of the camera.
;		'CENTER' = 2-Elements array of double. Boresight of the camera.
;		'KMAT': 3 by 2 array of doubles. Camera distortion coefficients matrix.
;		'BIGOMEGA' = Scalar double. Twist misalignment.
;		'EPSILON': 6-Elements array of doubles. Electromagnetic and optical 
;			distortion parameters vector.
;		'CMAT', 'CMAT_INI', 'CMAT_SAVED': 3 by 3 array of doubles. Rotation 
;			matrix corresponding to a specified unit quaternion.
;		'VOBS_STARS': Spacecraft velocity relative to the stars.	
;		
; COMMON BLOCKS:
;	CAVIAR_DATA
;
; MODIFICATIONS: 
;	2014, March		re-written by L-E. MEUNIER (IMCCE/OBSPM)
;-------------------------------------------------------------------------------
FUNCTION caviar_image_load, imgFile, imgType, $
							LABELS_FILE=lblFile, POINTING_FILE=pntgFile, GUI=gui
		
	;***************************************************************************
	; Test inputs:
	;***************************************************************************
	IF ISA(imgFile, 'String') EQ 0 || FILE_TEST(imgFile, /REGULAR) EQ 0 $
	THEN MESSAGE, "'IMGFILE' must be a string representing the image file full path."
	
        goodIMGtypes = ["CASSINI"]
	IF TOTAL(imgType EQ goodIMGtypes) NE 1 $
	THEN MESSAGE, "'IMGTYPE' must be one of these strings: '"+STRJOIN(goodImgTypes, "', '")+"'"
	
	
	
	;***************************************************************************
	; Load image's data:
	;***************************************************************************
	PRINT, FORMAT='(/A)', ">>>>> Start loading "+imgType+" image & header"

	;**********************************************
	; Read image's header from external file:
	IF ISA(lblFile, 'STRING') && FILE_TEST(lblFile, /REGULAR) THEN BEGIN
		PRINT, "Read image's header from file: ", lblFile
		
		; Extract image label from file to an array of the same number of lines:
		OPENR, lun, lblFile, /GET_LUN
		READF, lun, (header = STRARR(FILE_LINES(lblFile)))
		FREE_LUN, lun
		IF STRJOIN(header,/SINGLE) EQ '' $
		THEN MESSAGE, "Cannot extract header from file. File is empty."
		
		print, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx"
		
		; Determine header's type:
		labels = ["LINE_SAMPLES","LINES","Samples","Lines"]
		types = ['INT','INT','INT','INT']
		values = headerXtract(header, labels, types, "=", '')
		IF values[0] NE !NULL && values[1] NE !NULL THEN BEGIN
			hdrFormat='EXTVIC' & ns = values[0] & nl = values[1]
		ENDIF ELSE IF values[2] NE !NULL && values[3] NE !NULL THEN BEGIN
			hdrFormat='EXTCUB' & ns = values[2] & nl = values[3]
		ENDIF ELSE MESSAGE, "Cannot detect header format."
	ENDIF
	
	;***************************************
	; Extract image array & internal header:
	CASE imgType OF
                "CASSINI" : BEGIN
			; Construct a 'CassImg' object containing image and header with CassImg::Init()
			oImage = OBJ_NEW('CassImg');, DebugFlag=DebugFlag)  
			oImage->ReadVic, imgFile

			; Extract image raw data & convert to 8 bytes array
			rawimg = oImage->Image()

			; Extract image header
			IF header EQ !NULL THEN BEGIN
				header = oImage->LabelArray()
				IF header EQ !NULL || STRJOIN(header, /SINGLE) EQ '' $
                                THEN MESSAGE, "Cannot extract header from CASSINI image file."
				hdrFormat = "INTVIC"
			ENDIF
		END
		
	ENDCASE
	IF N_ELEMENTS(rawimg) LE 1 THEN MESSAGE, "Cannot read "+imgType+" image from file."
		
	;************************
	; Create image structure:
	imgSize = SIZE(rawimg)
	ns = imgSize[1] & nl = imgSize[2]
	image = CREATE_STRUCT('PATH', FILE_DIRNAME(imgFile), $
						  'NAME', FILE_BASENAME(imgFile), 'WINDOW', -1, $
						  'HEADER', header, 'HDRFRMT', hdrFormat, $
						  'RAW',rawimg, 'BYTE',BYTSCL(rawimg), 'NS',ns, 'NL',nl)
	
	PRINT, "<<<<< Image has been loaded succesfully!"
	;if (size(header))[0] EQ 2 THEN print, header ELSE foreach line, header do print, line9
	
	
	;***************************************************************************
	; Load generic SPICE kernels:
	;***************************************************************************
	PRINT, FORMAT='(/A)', ">>>>> Start getting generic SPICE kernels"
        caviar_loadspicekernels, GETENV('SPICEKERNEL_CASSINI'), loaded, TITLE="Select Cassini meta-kernel file:"

	IF loaded THEN PRINT, "<<<<<Cassini kernels have been loaded successfully!" $
	ELSE RETURN, 0
	
	;***************************************************************************
	; Load image information:
	;***************************************************************************
	; Get spacecraft ID from image header or by asking the user
	spcId = caviar_image_load_spcId(header, GUI=gui, FOUND=found)
	IF NOT found THEN BEGIN
		caviar_data_restore
		RETURN, 0
	ENDIF
	
	cspice_bodc2n, spcId, spcName, found
	
	PRINT, FORMAT='(/A)', ">>>>> Start loading "+spcName+"'s image parameters"
	params = caviar_getParams(spcId, header, hdrFormat, getCmatMethods)
	
  
	IF NOT ISA(params, 'STRUCT') THEN BEGIN
		caviar_data_restore
		RETURN, 0
	ENDIF ELSE image = CREATE_STRUCT(image, params)
	PRINT, "<<<<< Image parameters have been loaded successfully!"
	
	
	;***************************************************************************
	; Get image's other parameters from 'cameras_parameters.txt' file:
	;***************************************************************************
;njc 	cp = {FOVPIX:0D, FOCAL:0D, CENTER:DBLARR(2), KMAT:DBLARR(3,2), EPSILON:DBLARR(6)}
        cp = {FOVPIX:0D, FOCAL:0D, CENTER:DBLARR(2), KMAT:DBLARR(3,2), EPSILON:DBLARR(6), BIGOMEGA:0D}
	caviar_readCamParams, params.CAM.LNAME, cp

	fovimg = SQRT(ns*ns+nl*nl) * cp.fovpix * params.binning * !RADEG
	image = CREATE_STRUCT(image, 'FOCAL',cp.FOCAL, 'CENTER',cp.CENTER, 'KMAT',cp.KMAT, $
		'EPSILON',cp.EPSILON, 'FOVPIX',cp.FOVPIX, 'FOVIMG',fovimg, 'BIGOMEGA', cp.BIGOMEGA)
;njc                'EPSILON',cp.EPSILON, 'FOVPIX',cp.FOVPIX, 'FOVIMG',fovimg)

	
	
	;***************************************************************************
	; Get pointing matrix (cmat):
	;***************************************************************************
	cmat = 0
	FOREACH method, getCmatMethods DO BEGIN
		CATCH, error
		IF error THEN BEGIN
			PRINT, !ERROR_STATE.MSG_PREFIX+!ERROR_STATE.MSG
			CONTINUE
		ENDIF
		CASE method OF
			"FILE": IF ISA(pntgFile,'STRING') && pntgFile NE '' $
					THEN cmat = caviar_file2cmat(pntgFile)
			"SPICE": cmat = caviar_spice2cmat(image.SPC.ID, image.CAM.ID, image.CAM.LNAME, image.ET)
			"HEADER": cmat = caviar_lbl2cmat(image.SPC.ID, header, hdrFormat)
			ELSE: cmat = 0
		ENDCASE
		IF N_ELEMENTS(cmat) EQ 9 THEN BEGIN
			image = CREATE_STRUCT(image, 'CMAT',cmat, 'CMAT_INI',cmat, 'CMAT_SAVED',cmat)
			BREAK
		ENDIF
	ENDFOREACH
	IF N_ELEMENTS(cmat) NE 9 THEN BEGIN
		msg = "Cannot find camera pointing matrix."
		PRINT, msg
		res = DIALOG_MESSAGE(msg, /CENTER)
	ENDIF
	
	
	;***************************************************************************
	; Compute spacecraft velocity relative to the Solar System barycenter:
	; (which is equivalent to the velocity relative to the stars)
	;***************************************************************************
	CATCH, error
	IF error NE 0 THEN BEGIN
		pos = STRPOS(!ERROR_STATE.MSG, ']')
		PRINT, !ERROR_STATE.MSG_PREFIX+STRMID(!ERROR_STATE.MSG, 0, pos+1)
		l = STRSPLIT(STRMID(!ERROR_STATE.MSG, pos+2), '(  +)', /REGEX, /EXTRACT)
		l[0] = STRMID(l[0], 0, STRLEN(l[0])-1) 
		PRINT, '  '+STRJOIN(l, ' ')
		PRINT, "Cannot get spacecraft velocity relative to stars. A null velocity will be set!"
		image = CREATE_STRUCT(image, 'VOBS_STARS', [0D,0D,0D])
	ENDIF ELSE BEGIN
		cspice_spkez, image.SPC.ID, image.ET, 'J2000', 'NONE', 0L, state, ltime
		image = CREATE_STRUCT(image, 'VOBS_STARS', state[3:5])
	ENDELSE
	
	RETURN, image
END


;======================================================================
;add several functions for autoprocessing, Zhangqf,  Aug. 2021

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: getParasFromPointing
; PURPOSE:
;   
; version:
;  added by zhangqf, Aug, 2021
;-------------------------------------------------------------------------------
FUNCTION getParasFromPointing, file
  COMMON CAVIAR_DATA, image, catstars, imgstars, planets
  COMMON CAVIAR_LOG, logfileName, logFileID, resFileName, resFileID

  IF NOT FILE_TEST(file, /REGULAR) THEN MESSAGE, "Input file does not exist."

  nlines = FILE_LINES(file)
  OPENR, lun, file, /GET_LUN
  READF, lun, ( lines = STRARR(nlines) )
  FREE_LUN, lun
  
  index = WHERE(STREGEX(lines,'FIND_STAR_PARAMETERS = ', /BOOLEAN), count)
  ;FIND_STAR_PARAMETERS = fwhm:1.3 nsig:3 roundlim: [-1, 1] sharplim: [0.2, 1]
  if count NE 1 THEN MESSAGE, 'There are no or more than 1 find_star_paremeters.'
  lineParts = STRSPLIT(lines[index],' []:',/EXTRACT)
  nsig1=double(lineparts[5])
  roundlim1=double(lineparts[7:8])
  sharplim1=double(lineparts[10:11])
  find_parameters={FWHM: 1.3, NSIG:nsig1, ROUNDLIM: roundlim1, SHARPLIM:sharplim1 }
  
  return, find_parameters
END


;obtain observaton position and pointing from pointing file.
;if pointing file have no the observation record, then the funciont cann't be used.
;get the values of
; obvX, obvY, obvRa, obvDec
; Note: in QMPF file, the values about satellite have been output in the following 
; SATELLITE = targetname ycent, xcent, ra, dec, sigma, method, offsety, offsetx ..
; for exmple:
;SATELLITE = PHOEBE  508.0762  455.1723  28.292168301702   6.400198782066      0.500 CENTROID    -0.121     0.051
FUNCTION getObservations, FileQMPF

  IF NOT FILE_TEST(FileQMPF, /REGULAR) THEN MESSAGE, "QMPF file does not exist."

  openr, lun, FileQMPF, /get_lun
  line=''
  WHILE ~ EOF(lun) DO BEGIN
    READF, lun, line

    IF isa(line, /STRING) THEN BEGIN
      IF ( strmatch(line,'SATELLITE = PHOEBE*', /FOLD_CASE) EQ 1 ) THEN BEGIN
        vals=STRSPLIT(line, /EXTRACT)
        ;SATELLITE = ENCELADUS LIMB FITTING  529.3901  517.4434  74.893171516934  -5.040501354243      0.165     0.000     0.000
        ;SATELLITE = PHOEBE  508.0762  455.1723  28.292168301702   6.400198782066      0.500 CENTROID    -0.121     0.051
        strTarget=vals[2]
        strMethod=vals[8]
        yOBV=double(vals[3])
        xOBV=double(vals[4])
        RaOBV=double(vals[5])   ;observation in celesphere is obtained from file, not computed from pointing file.
        DecOBV=double(vals[6])
        offsetx=double(vals[10])
        offsety=double(vals[9])
      ENDIF

      IF (  strmatch(line,'POINTING_RA =*', /FOLD_CASE) EQ 1 ) THEN BEGIN
        vals=STRSPLIT(line, /EXTRACT)
        pointing_ra=double(vals[2])
      ENDIF
      IF (  strmatch(line,'POINTING_DEC =*', /FOLD_CASE) EQ 1  ) THEN BEGIN
        vals=STRSPLIT(line, /EXTRACT)
        pointing_dec=double(vals[2])
      ENDIF

      IF (  strmatch(line,'POINTING_TWIST =*', /FOLD_CASE) EQ 1  ) THEN BEGIN
        vals=STRSPLIT(line, /EXTRACT)
        pointing_twist=double(vals[2])
      ENDIF

      IF (  strmatch(line,'POINTING_TWIST(GLL) =*', /FOLD_CASE) EQ 1  ) THEN BEGIN
        vals=STRSPLIT(line, /EXTRACT)
        pointing_gll=double(vals[2])
      ENDIF
      
      IF (  strmatch(line,'POINTING_QUATERNION*', /FOLD_CASE) EQ 1  ) THEN BEGIN
        vals=STRSPLIT(line, /EXTRACT)
        ;POINTING_QUATERNION =   0.1738895607116  0.6442828846204  0.1702806054227  0.7250286205921
        cmat=double(vals[2:5])
        
      ENDIF

    ENDIF

  ENDWHILE
  close,lun
  free_lun, lun

  rtn={Target:strTarget, Method:strMethod, OBVx:xOBV, OBVy:yOBV, OBVRa:RaOBV, OBVDec:DecOBV,  $
    SIGMA: 0.5, OFFX:offsetx, OFFY: offsety,PNTRA:pointing_ra,PNTDEC:pointing_dec, $
    PNTTWIST:pointing_twist, PNTTGLL:pointing_gll, CMAT:cmat}
  return, rtn
END


PRO loadGaiaEDR3
  COMMON CAVIAR_DATA, image, catstars, imgstars, planets
  COMMON CAVIAR_LOG, logfileName, logFileID, resFileName, resFileID
  
  ;***************************************************************************
  ; Convert ephemeris time (TDB seconds past J2000) to epoch (UTC decimal years):
  ;***************************************************************************
  cspice_timout, image.ET, 'YYYY.#########::UTC', 14, ctime
  epoch = DOUBLE(ctime)
  ;***************************************************************************
  ; Get pointing in RA, DEC from cmat:
  ;***************************************************************************
  rho_J2000 = image.CMAT#[0.0d0, 0.0d0, 1.0d0]  ;rho_J2000 = cmat # rho_CAMERA
  cspice_recrad, rho_J2000, range, pRA, pdec
  
  fovsearch=image.fovimg
  magmin=-2
  magmax=14
  res= caviar_catstars_ctlgaccess( 'GAIAE3', epoch, pRA, pDEC, fovsearch, magmin, magmax)
  
  tmpRA=!NULL & tmpDec = !NULL
  for i=0 , n_elements(res.Name)-1 do begin
    tmpRA=[tmpRA, res.ra[i]]
    tmpDec=[tmpDec, res.dec[i]]
  endfor

  radec2slcoord, tmpRA, tmpDec, sample, line, CORRECT_STELAB=image.VOBS_STARS
  
  catStars = LIST()
  for k = 0, n_elements(sample)-1 do begin
    catStars.add, {NAME:res.name[k], MAG:res.mag[k], RA:res.ra[k], DEC:res.dec[k], $
      XCOORD:sample[k], YCOORD:line[k], MATCHED:0, FITTED:0}
  endfor

END


;;--------------------------------------------------------
;; adapt the caviar_image_load to caviar_image_load_A for batchprocessing
;; modified by zhangqf Dec 24, 2017
;-------------------------------------------------------------------------------
FUNCTION CAVIAR_IMAGE_LOAD_A, imgFile, imgType, $
  LABELS_FILE=lblFile, POINTING_FILE=pntgFile, GUI=gui

  ;***************************************************************************
  ; Load image's data:
  ;***************************************************************************
  PRINT, FORMAT='(/A)', ">>>>> Start loading "+imgType+" image & header"

  oImage = OBJ_NEW('CassImg');, DebugFlag=DebugFlag)
  oImage->ReadVic, imgFile

  ; Extract image raw data & convert to 8 bytes array
  rawimg = oImage->Image()

  ; Extract image header
  IF header EQ !NULL THEN BEGIN
    header = oImage->LabelArray()
    IF header EQ !NULL || STRJOIN(header, /SINGLE) EQ '' $
      THEN MESSAGE, "Cannot extract header from VICAR image file."
    hdrFormat = "INTVIC"
  ENDIF

  IF N_ELEMENTS(rawimg) LE 1 THEN MESSAGE, "Cannot read "+imgType+" image from file."

  ;************************
  ; Create image structure:
  imgSize = SIZE(rawimg)
  ns = imgSize[1] & nl = imgSize[2]
  image = CREATE_STRUCT('PATH', FILE_DIRNAME(imgFile), $
    'NAME', FILE_BASENAME(imgFile), 'WINDOW', -1, $
    'HEADER', header, 'HDRFRMT', hdrFormat, $
    'RAW',rawimg, 'BYTE',BYTSCL(rawimg), 'NS',ns, 'NL',nl)

  PRINT, "<<<<< Image have been loaded succesfully!"


  spcId=-82
  cspice_bodc2n, spcId, spcName, found

  PRINT, FORMAT='(/A)', ">>>>> Start loading "+spcName+"'s image parameters"
  params = caviar_getParams(spcId, header, hdrFormat, getCmatMethods)

  IF NOT ISA(params, 'STRUCT') THEN BEGIN
    RETURN, 0
  ENDIF ELSE image = CREATE_STRUCT(image, params)
  PRINT, "<<<<< Image parameters have been loaded succesfully!"

  ;***************************************************************************
  ; Get image's other parameters from 'cameras_parameters.txt' file:
  ;***************************************************************************
  cp = {FOVPIX:0D, FOCAL:0D, CENTER:DBLARR(2), KMAT:DBLARR(3,2), EPSILON:DBLARR(6), BIGOMEGA:0D}
  caviar_readCamParams, params.CAM.LNAME, cp

  fovimg = SQRT(ns*ns+nl*nl) * cp.fovpix * params.binning * !RADEG
  image = CREATE_STRUCT(image, 'FOCAL',cp.FOCAL, 'CENTER',cp.CENTER, 'KMAT',cp.KMAT, $
    'EPSILON',cp.EPSILON, 'FOVPIX',cp.FOVPIX, 'FOVIMG',fovimg, 'BIGOMEGA', cp.BIGOMEGA)
  ;njc                'EPSILON',cp.EPSILON, 'FOVPIX',cp.FOVPIX, 'FOVIMG',fovimg)


  ;***************************************************************************
  ; Get pointing matrix (cmat):
  ;***************************************************************************
  cmat = 0
  cmat = caviar_file2cmat(pntgFile)  ;这里先初步准备数据，后面再进一步处理。
  ;cmat = caviar_spice2cmat(image.SPC.ID, image.CAM.ID, image.CAM.LNAME, image.ET)
  
  IF N_ELEMENTS(cmat) EQ 9 THEN BEGIN
    image = CREATE_STRUCT(image, 'CMAT',cmat, 'CMAT_INI',cmat, 'CMAT_SAVED',cmat)
  ENDIF

  ;***************************************************************************
  ; Compute spacecraft velocity relative to the Solar System barycenter:
  ; (which is equivalent to the velocity relative to the stars)
  ;***************************************************************************
  cspice_spkez, image.SPC.ID, image.ET, 'J2000', 'NONE', 0L, state, ltime
  image = CREATE_STRUCT(image, 'VOBS_STARS', state[3:5])

  RETURN, image
  
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: DayYear2Julianday
; PURPOSE: convert format of "day of year" to Julian day.
;      for example, "2013-018T07:10:46.404" will be changed to 2456310.799148194
; Input: dayYear: a string of the format of "day of  year"
;        for exmaple,  "2013-018T07:10:46.404"
; output: string of Julainday, by the format %s14.6
;       for example, the Julianday is  2456310.799148194,
;       it will output "2456310.799148"
;       At the same time, the string started with year will return, 
;       for example, "2013-018T07:10:46.404" is changed to 2013.0473949.
;       where,  .0473949 denote the place of "018T07:10:46.404" in year 2013.
;       The value is used to draw.
;-------------------------------------------------------------------------------
FUNCTION DayYear2Julianday, dayYear

  v=strsplit(dayYear, "T-:",/EXTRACT )
  year=fix(v[0])
  day=fix(v[1])
  hh=fix(v[2])
  mm=fix(v[3])
  ss=double(v[4])
  jd=julday(1,day,year, hh, mm, ss)
  strjd1= string(jd, FORMAT = '(F14.6)')
  
  jd0=julday(1,1,year, 0, 0, 0.0)
  jd1=julday(1,1,year+1, 0, 0, 0.0)
  tmp=year+(jd-jd0)/(jd1-jd0)
  strjd2= string(tmp, FORMAT = '(F12.5)')
  return, [strjd1, strjd2]
END

; added by zhanqf, Oct, 2021
; output all results into the same file.
; OUTPUT RESULTS BY MY FORMAT.
;  理论位置/观测位置/相位差
FUNCTION OUTPUT1,  obvPnt, cmp
  COMMON CAVIAR_DATA, image, catstars, imgstars, planets
  COMMON CAVIAR_LOG, logfileName, logFileID, resFileName, resFileID

  
  n=0
  ;FOR i=0, N_ELEMENTS(catstars)-1 DO  IF (catstars[i]).fitted EQ 1 THEN  n++
  
  rao=obvPnt.PNTRA
  deco=obvPnt.PNTDEC
  twisto=obvPnt.PNTTWIST
  twistogll =obvPnt.PNTTGLL
  cmat_quat= obvPnt.CMAT
  xoff=obvPnt.OFFX
  yoff=obvPnt.OFFY
  
  obx=obvPnt.OBVX
  oby=obvPnt.OBVY
  obRA=obvPnt.OBVRa
  obDEC=obvPnt.OBVDec
   
  SpiceRa = cmp.RA * !RADEG    ; in degree
  SpiceDec = cmp.DEC * !RADEG  ; in degree
  
  resx=obx-cmp.XCOORD
  resy=oby-cmp.YCOORD
  
  
  ; Residual (O-C) in Ra*cos(dec) and dec by arcsecond and kilometers
  resRcD = (obRA -SpiceRa)*cos(cmp.DEC) * 3600D  ; in arcsecond in Ra*cos(dec) direction
  resDec = (obDEC - SpiceDec)* 3600D           ; in arcsecond in declination
  reskmRcD = resRcD/1.2354D * cmp.PIXSIZE        ; in kilometer in Ra*cos(dec) direction
  reskmDec = resDec/1.2354D * cmp.PIXSIZE        ; in kilometer in declination
  
  ;compute Julian day
  ii=WHERE(STREGEX( image.header, 'IMAGE_MID_TIME', /BOOLEAN, /FOLD_CASE ), /NULL)
  strMidTime=image.header[ii+1]
  if (strupcase( strmid(strMidTime, 0, /reverse_offset) ) EQ 'Z' ) then $
    strMidTime=strmid(strMidTime, 0, strlen(strMidTime)-1)

  strjd=DayYear2Julianday(strMidTime)

;  print, "before printf"
;  print, image.Name, strMidTime, cmp.NAME, cmp.PIXSIZE
;  print, ObRa, ObDec , obx, oby, 0.5D, xoff, yoff
;  print, SpiceRa, SpiceDec
;  print, cmp.XCOORD, cmp.YCOORD  
;  print,resx, resy, resRcD, resDec, reskmRcD, reskmDec 
;  print, rao, deco, twisto , twistogll
;  print,  cmat_quat[0], cmat_quat[1], cmat_quat[2], cmat_quat[3]
;  print, strjd[0], strjd[1]
;  read, ff1
  PRINTF, resFileID, $
      FORMAT='(4(A, X),F9.3,X,2(F0.7,X),5(F0.4,X),2(F0.7,X),8(F0.5,X),(A, A,X), A, 4(d17.12,2X), (A27, 4(D17.13),2X),(A,I4,2X), 4(A,X))', $
      image.Name, strMidTime, cmp.NAME, "CTRDMM", cmp.PIXSIZE, $
      ObRa, ObDec , obx, oby, 0.5D, xoff, yoff, $
      SpiceRa, SpiceDec, cmp.XCOORD, cmp.YCOORD,  resx, resy, resRcD, resDec, reskmRcD, reskmDec, $
      "Reference_Catalogue = ", "GAIAEDR3", $
      "POINTING= ",rao, deco , twisto , twistogll, $
      "POINTING_QUATERNION = ", cmat_quat[0], cmat_quat[1], cmat_quat[2], cmat_quat[3], $
      "Number_Reference_Stars = ", n, "Julian_Day1 = ", strjd[0], "Julian_Day2 = ", strjd[1]

   RETURN, 1
END



;;------------------------------------------------------------
;; added by zhangqf, April, 2021
;;
;;-------------------------------------------------------------
;+
; :Description:
;     For batch processing of measuring satellite.
;
;
; :Author: zhang
;-------------------------------------------------------------------
function caviar_ProcessIMG
  COMMON CAVIAR_DATA, image, catstars, imgstars, planets
  COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
  COMMON CAVIAR_LOADDATA, imgType, imgFile, lblType, lblFile, pntgFile

  COMMON CAVIAR_LOG, logfileName, logFileID, resFileName, resFileID

  ; step 1: loading image
  image = CAVIAR_IMAGE_LOAD_A(imgFile, imgType, POINTING_FILE=pntgFile, /GUI)

  
  imgFileName=image.name
  IMGMIDTIME = HEADERXTRACT(image.HEADER, 'IMAGE_MID_TIME', 'STRING', '', '' )
  STRMIDTIME= IMGMIDTIME[0]
  IF (strupcase( strmid(strMidTime, 0, /reverse_offset) ) EQ 'Z' ) THEN $
    strMidTime=strmid(strMidTime, 0, strlen(strMidTime)-1)
  ;IF ( STRMIDTIME.ENDSWITH('Z') ) THEN STRMIDTIME=STRMIDTIME.REMOVE(-1)
  
  ;step 2: load relevant satellites.
  CAVIAR_SATELLITES_LOAD, 699  ; load the variable planets，这时候已经得到理论值了
  cmp=planets[9]    ;Phoebe is object

  ; step 3: loading observation values
  ObvPnt= getObservations(pntgFile)

  
  ; Step 4: Output all result to one specific file
  r1=output1(obvPnt, cmp)
  
  return,1
END

;added by zhangqf, Aug. 2021
;For batch processing of compute the residuals relative to MCCE Ph20.
FUNCTION Batch4PH20
  COMMON CAVIAR_DATA, image, catstars, imgstars, planets
  COMMON CAVIAR_LOADDATA, imgType, imgFile, lblType, lblFile, pntgFile
  COMMON CAVIAR_STATUS, SpiceKernelLoaded
  COMMON CAVIAR_LOG, logfileName, logFileID, resFileName, resFileID

  ;***************************************************************************
  ; Load generic SPICE kernels:
  ;***************************************************************************
  PRINT, FORMAT='(/A)', ">>>>> Start getting generic SPICE kernels"

  caviar_loadspicekernels, GETENV('SPICEKERNEL_CASSINI'), loaded, TITLE="Select generic's meta-kernel file:"
  IF loaded THEN PRINT, "<<<<< Generic kernels have been loaded succesfully!"

  ;***************************************************************************
  ; process every cassini image:
  ;***************************************************************************
  imglistFile= '/home/zhang/temp/phoebe/imagelist.txt'
  images_path='/home/zhang/data/Phoebe/'
  pnt_path= '/home/zhang/temp/phoebe/QMPFs/'
  
  
  nlines = FILE_LINES(imglistFile)
  
  OPENR, lun, imglistFile, /GET_LUN
  READF, lun, ( imglist= STRARR(nlines) )
  FREE_LUN, lun
  
  
  
  logfilename='/home/zhang/temp/phoebe/logPh20B.txt'
  resFileName='/home/zhang/temp/phoebe/Results-PH20B-zhang.txt'

  ;得到所有的nac图像。
  
  openw, logFileID, logfilename,/GET_LUN, /APPEND
  print, "logfilename is ", logfilename

  openw, resFileID, resFileName,/GET_LUN, /APPEND
  print, "Results filename is ", resFileName
  

  ;phasefile="/home/zhang/temp/phasecorrection.txt"
  ;OPENW, phaselun, phasefile, /GET_LUN, /APPEND

  FOR i=0, nlines-1 DO BEGIN
    file=file_search(images_path, imglist[i], COUNT=cn)
    if (cn LT 1) then begin
      print, imglist[i], cn
      read, ff1
    endif
    imgFile=file[0]

    aa=strsplit(imglist[i],'.',/EXTRACT)
    bb=aa[0]+'*.QMPF'
    file=file_search(pnt_path, bb, COUNT=cn)
    if (cn NE 1) then begin
      print, imglist[i], 'qmpf files', cn
      read, ff1
    endif
    pntgFile=file[0]

 
    IMGTYPE='VICAR'

    print, "Pointing file is  ", pntgFile

    printf, logFileID, systime()
    printf, logFileID, " NAC image Name: ", imgFile
    printf, logFileID, "pointing file name is ", pntgFile

    
    r1=caviar_ProcessIMG( )
    print, "imgInd =", i

  endfor

  print, "Have finished all files!"
  close, logFileID
  FREE_LUN, logFileID
  close, resFileID
  FREE_LUN, resFileID

  RETURN, 1  ; if the sentence is missed, one error will be caused.
end

;=====================================================================================


