@caviar_satModel_spice.pro

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SHAPEMODEL_MOVE
; PURPOSE: Update limb model position considering the shift/angle action
;-------------------------------------------------------------------------------
PRO caviar_satModel_move, TRANSLATE=shift, ROTATE=angle
	
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	
	IF KEYWORD_SET(shift) THEN BEGIN
		shift /= imgDraw.ZFACTOR
		shift[1] = -shift[1]
	ENDIF
	
	IF KEYWORD_SET(angle) THEN BEGIN	
		; Get the rotation center:
		slCenter = haveTag(satModel, 'SLCENTER') ? satModel.slCenter : [[0],[0]]
		
		; Get the rotation matrix:
		angle = -angle
		R = [[ cos(angle), -sin(angle)], $
			 [ sin(angle),  cos(angle)]]
			 
		; Compute image origin rotated from angle relative to the center:
		rOri = ( DIAG_MATRIX([1,1]) - R ) ## slCenter
	ENDIF
	
	
	IF haveTag(satModel, 'SLCENTER') THEN BEGIN
		IF KEYWORD_SET(shift) THEN satModel.slCenter += shift
		
		; Update ra/dec coordinates:
		slcoord2radec, satModel.slCenter[0], satModel.slCenter[1], ra, dec, ITERATE=3
		satModel.rdCenter = [[ra], [dec]]
	ENDIF
	IF haveTag(satModel, 'SLLIMB') THEN BEGIN
		npts = (SIZE(satModel.slLimb))[1]
		
		IF KEYWORD_SET(shift) $
		THEN satModel.slLimb += TRANSPOSE(shift) ## MAKE_ARRAY(npts, VALUE=1.0D)
		
		IF KEYWORD_SET(angle) $
		THEN satModel.slLimb = rOri ## MAKE_ARRAY(npts, VALUE=1.0D) + R ## satModel.slLimb
		
		; Update ra/dec coordinates:
		slcoord2radec, satModel.slLimb[*,0], satModel.slLimb[*,1], ra, dec, ITERATE=1
		satModel.rdLimb = [[ra], [dec]]
	ENDIF
	IF haveTag(satModel, 'SLTERM') THEN BEGIN
		npts = (SIZE(satModel.slTerm))[1]
		
		IF KEYWORD_SET(shift) $
		THEN satModel.slTerm += TRANSPOSE(shift) ## MAKE_ARRAY(npts, VALUE=1.0D)
		
		IF KEYWORD_SET(angle) $
		THEN satModel.slTerm = rOri ## MAKE_ARRAY(npts, VALUE=1.0D) + R ## satModel.slTerm
		
		; Update ra/dec coordinates:
		slcoord2radec, satModel.slTerm[*,0], satModel.slTerm[*,1], ra, dec, ITERATE=1
		satModel.rdterm = [[ra], [dec]]
	ENDIF
	IF haveTag(satModel, 'SLEQUA') THEN BEGIN
		npts = (SIZE(satModel.slEqua))[1]
		
		IF KEYWORD_SET(shift) $
		THEN satModel.slEqua += TRANSPOSE(shift) ## MAKE_ARRAY(npts, VALUE=1.0D)
		
		IF KEYWORD_SET(angle) $
		THEN satModel.slEqua = rOri ## MAKE_ARRAY(npts, VALUE=1.0D) + R ## satModel.slEqua
		
		; Update ra/dec coordinates:
		slcoord2radec, satModel.slEqua[*,0], satModel.slEqua[*,1], ra, dec, ITERATE=1
		satModel.rdEqua = [[ra], [dec]]
	ENDIF
	
	; Update display
	caviar_display
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SHAPEMODEL_LOAD
; PURPOSE: Load a shape model for the satellite by using one of these methods:
;	- Get an ellipsoid model from SPICE kernels and display it.
; - 从SPICE内核获得一个DSK模型并且展现它。 孙德淦，2022.9.20
; INPUTS:
; - 边缘参数：4元数组。
; - 边缘检测器参数：中值滤波器宽度和canny算法高低阈值和sigma。
;	edParams: 4-Elements array. Edge detector parameters: median filter width 
;			  and canny algorithm high and low threshold and sigma.
;-------------------------------------------------------------------------------
PRO caviar_satModel_load, method
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	COMMON CAVIAR_GUI, wCAVIARtlb, wFITSATLIMBbase
		
	; debug, PASS, GUI页面调用caviar_satModel_load，能正确识别method
;	CASE method of
;	  0: BEGIN
;	    debug = DIALOG_MESSAGE('ELL', title = 'ELL', /Information)
;	    WIDGET_CONTROL, debug, /realize
;	  END
;	  1: BEGIN
;	    debug = DIALOG_MESSAGE('DSK', title = 'DSK', /Information)
;	    WIDGET_CONTROL, debug, /realize
;	  END
;	  ELSE: PRINT, modelindex
;	ENDCASE
	
	; /CHILD:设置此关键字以返回由Widget_ID指定的部件的第一个子部件的ID。 如果小部件没有孩子，则返回0。
	; 显示边缘拟合组件的第一个孩子组件，并获得结构体指针pstate
	WIDGET_CONTROL, WIDGET_INFO(wFITSATLIMBbase, /CHILD), GET_UVALUE=pstate
	; 获取extra组件中state结构体的数据
	npts = (*pstate).satModelNpts    ; 卫星模型边缘点数量，默认为1000个点
	vals = (*pstate).smcpVals        ; 不知道代表什么意思，smcp是一个结构体，这里vals是一个数组[3,1.0,0.4,0.6]，加载模型的时候没用到
	pid = (planets[selSatIndex]).ID  ; 从caviar_data中获取行星的卫星ID
	
	CASE method OF
	 ; method为0则加载三维椭球模型
		0: BEGIN
		satModel = caviar_satModel_spice(pid, image.et, image.SPC.ID, npts)
		; debug,PASS,能正确识别method为0
;		debug = DIALOG_MESSAGE('ELL', title = 'ELL', /Information)
;		WIDGET_CONTROL, debug, /realize
		END
		
		; @孙德淦，2022.9.22，增加了加载DSK模型的功能，method为1则加载DSK模型, 调用caviar_satModel_spice_dsk函数加载
		1: BEGIN
		  satModel = caviar_satModel_spice_dsk(pid, image.et, image.SPC.ID, npts)
		  ; debug，PASS，能正确识别method为1
;		  debug = DIALOG_MESSAGE('DSK', title = 'DSK', /Information)
;		  WIDGET_CONTROL, debug, /realize
		END
	ENDCASE		
END

