@caviar_satModel_pkg.pro
@caviar_satLimbFit_ite.pro
@caviar_satLimbFit_lsq.pro
@caviar_xmove.pro
@caviar_edgrad_pkg.pro
@caviar_edcanny_pkg.pro

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	List of procedure of this file:
;	- PRO		caviar_satlimbfit_gui_init
;	- PRO 		caviar_satlimbfit_gui_event
;	- PRO 		caviar_satlimbfit_gui
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATLIMBFIT_GUI_INIT
; PURPOSE: Set default values/labels/parameters/etc. of the GUI.
;-------------------------------------------------------------------------------
PRO caviar_satlimbfit_gui_init
	
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	
	; Delete image filtering window:
	DEVICE, WINDOW_STATE=winState
	IF winState[31] NE 0 THEN WDELETE, 31
	
	; Initialize data:
	satModel = {}
	satLimb = {}
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	                  Manage events of the widget 		     				   
;-------------------------------------------------------------------------------
PRO caviar_satlimbfit_gui_event, event
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
		RETURN	
	ENDIF
	
	; Get state from the first child of the compound widget root:
	child = WIDGET_INFO(event.HANDLER, /CHILD)
	WIDGET_CONTROL, child, GET_UVALUE=pstate
	
	WIDGET_CONTROL, /HOURGLASS

	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
						
		;***********************************************************************
		; Satellite shape model (limb, terminator, equator):
		;***********************************************************************
		"SHAPE MODEL SELECT": BEGIN
			IF selSatIndex EQ !NULL THEN BREAK
			WIDGET_CONTROL, /HOURGLASS
			; /DROPLIST_SELECT:此关键字适用于使用WIDGET_DROPLIST函数创建的小部件。 
			; 设置此关键字以返回指定下拉列表小部件中当前选定元素（即当前显示的元素）的从零开始的编号。
			; modelindex即获得选择的是三维椭球模型还是DSK模型，0为三维椭球模型，1为DSK模型
			modelindex = WIDGET_INFO((*pstate).wSMdlist, /DROPLIST_SELECT)
			
			; debug, pass，modelindex具有0,1变化
;			IF modelindex EQ 0 THEN BEGIN
;			   debug = DIALOG_MESSAGE('ELL', title = 'ELL', /Information)
;         WIDGET_CONTROL, debug, /realize
;			ENDIF ELSE BEGIN
;			   debug = DIALOG_MESSAGE('DSK', title = 'ELL', /Information)
;         WIDGET_CONTROL, debug, /realize
;			ENDELSE

			; Change the "visible" parameters base to follow the droplist selection:
			; 更改“可见”参数基础以遵循下拉列表选择：
			FOR i=0,N_ELEMENTS((*pstate).wSMPARAMSbases)-1 $
			 ; 模型索引和参数索引相同，则MAP设置为1，参数组件在屏幕上显示（可见）;
			 ; 如果模型索引和参数索引不相同，则MAP设置为0,该参数组件从屏幕上消失（不可见）。
			DO WIDGET_CONTROL, (*pstate).wSMPARAMSbases[i], MAP = i EQ modelindex ? 1 : 0
			
			; Resize parameters base & change the button title:
			; 调整参数大小并更改按钮标题：
			(*pstate).wSMPbaseYsize= ([1,35,110])[modelindex+1]
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
			WIDGET_CONTROL, (*pstate).wSMPbase, SCR_YSIZE=(*pstate).wSMPbaseYsize
			WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
			WIDGET_CONTROL, (*pstate).wSMPARAMSbttn, SET_VALUE='-', SET_UVALUE='+'
			
			; Load shape model:
			; 调用加载模型过程，给它传递模型索引modelindex。
			; modelindex = 0表示加载三维椭球模型，modelindex = 1表示加载DSK模型。
			caviar_satModel_load, modelindex
			
			caviar_display
		END
		
		
		"SHAPE MODEL LOAD": BEGIN
			IF selSatIndex EQ !NULL THEN BREAK
			
			WIDGET_CONTROL, /HOURGLASS
			
			modelindex = WIDGET_INFO((*pstate).wSMdlist, /DROPLIST_SELECT)
			
			; debug, PASS，加载模型可以正确识别modelindex
;      IF modelindex EQ 0 THEN BEGIN
;        debug = DIALOG_MESSAGE('ELL', title = 'ELL', /Information)
;         WIDGET_CONTROL, debug, /realize
;      ENDIF ELSE BEGIN
;        debug = DIALOG_MESSAGE('DSK', title = 'DSK', /Information)
;         WIDGET_CONTROL, debug, /realize
;      ENDELSE
     
			caviar_satModel_load, modelindex
			caviar_display
		END
		
		"SHAPE MODEL MOVE": BEGIN
			IF NOT haveTag(satModel, 'SLLIMB') THEN BEGIN
				msg = "Please load a shape model first!"
				res = DIALOG_MESSAGE(msg, /CENTER, DIALOG_PARENT=event.ID)
				RETURN
			ENDIF
			
			; Change parameters button value:
			WIDGET_CONTROL, (*pstate).wSMPARAMSbttn, GET_VALUE=curVal
			WIDGET_CONTROL, (*pstate).wSMPARAMSbttn, SET_VALUE='-', SET_UVALUE='+'
						
			; Resize parameters base:
			(*pstate).wSMPbaseYsize = 120
			WIDGET_CONTROL, (*pstate).wSMPbase, SCR_YSIZE=(*pstate).wSMPbaseYsize
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
			WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
			
			print, (*pstate).wSMPARAMSBases[2]
			
			FOR i=0,N_ELEMENTS((*pstate).wSMPARAMSBases)-1 $
			DO WIDGET_CONTROL, (*pstate).wSMPARAMSBases[i], MAP = i EQ 2 ? 1 : 0
		END
		
;		"MOVE": BEGIN
;			CASE event.type OF
;				1:  caviar_satModel_move, TRANSLATE = event.value
;				2:  caviar_satModel_move, TRANSLATE = event.value
;				3:  caviar_satModel_move, TRANSLATE = event.value
;				4:  caviar_satModel_move, TRANSLATE = event.value
;				5:  BEGIN
;						WIDGET_CONTROL, /HOURGLASS
;						;caviar_matchStars_mouseMove, TFACTOR=event.value.tFactor, $
;						;	  						 RFACTOR=event.value.rFactor
;					END	
;				6:  caviar_satModel_move, TRANSLATE = event.value
;				7:  caviar_satModel_move, TRANSLATE = event.value
;				8:  caviar_satModel_move, TRANSLATE = event.value
;				9:  caviar_satModel_move, TRANSLATE = event.value
;				11: caviar_satModel_move, ROTATE = event.value*!DTOR
;				12: caviar_satModel_move, ROTATE = event.value*!DTOR
;				ELSE:
;			ENDCASE
;		END
		
		"SHAPE MODEL PARAMS": BEGIN
			; Change the button title from "+" to "-" or vice cersa:
			; 将按钮标题从“+”更改为“-”：
			WIDGET_CONTROL, event.ID, GET_VALUE=curVal, GET_UVALUE=newVal
			WIDGET_CONTROL, event.ID, SET_VALUE=newVal, SET_UVALUE=curVal
				
			; Resize parameters base:
			; 调整参数库：
			visible = curVal EQ '+'
			index = WIDGET_INFO((*pstate).wSMdlist, /DROPLIST_SELECT)
			(*pstate).wSMPbaseYsize = ([1,35,140])[visible*(index+1)]
			WIDGET_CONTROL, (*pstate).wSMPbase, SCR_YSIZE=(*pstate).wSMPbaseYsize
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
						WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
						
			FOR i=0,N_ELEMENTS((*pstate).wSMPARAMSBases)-1 $
			DO WIDGET_CONTROL, (*pstate).wSMPARAMSBases[i], MAP = i EQ index ? 1 : 0
		END
		
		; Parameters:
		; 三维椭球模型参数
		"SM ELLIPSOID NPOINTS": BEGIN
			oldVal = (*pstate).satModelNpts
			(*pstate).satModelNpts = DOUBLE(event.str)
			IF ARRAY_EQUAL(DOUBLE(event.str), oldVal) EQ 0 && (*pstate).smAutoUpdt $
			THEN caviar_satModel_load, 0
			caviar_display
		END
		
		; @孙德淦，2022.9.20
		; DSK模型边缘点数量参数的响应事件
		"SM DSK NPOINTS": BEGIN
		  oldVal = (*pstate).satModelNpts
		  (*pstate).satModelNpts = DOUBLE(event.str)
		  IF ARRAY_EQUAL(DOUBLE(event.str), oldVal) EQ 0 && (*pstate).smAutoUpdt $
		  THEN caviar_satModel_load, 1
		  caviar_display
		END
		
		
		"SM AUTO UPDATE": (*pstate).smAutoUpdt = event.select
		
		;***********************************************************************
		; Satellite Limb Detection:
		;***********************************************************************
		"LIMB DETECTION METHOD": BEGIN
			IF selSatIndex EQ !NULL THEN BREAK
			WIDGET_CONTROL, /HOURGLASS
			methodindex = WIDGET_INFO((*pstate).wLDdlist, /DROPLIST_SELECT)
			
			; Change the mapped base for parameters to follow the droplist selection:
			wLDMsbases = (*pstate).wLDMsbases
			FOR i=0, N_ELEMENTS(wLDMsbases)-1 $
			DO WIDGET_CONTROL, wLDMsbases[i], MAP = i EQ methodindex ? 1 : 0
			
			WIDGET_CONTROL, wLDMsbases[methodindex], GET_UVALUE=ysize
			WIDGET_CONTROL, WIDGET_INFO(wLDMsbases[methodindex], /PARENT), SCR_YSIZE=ysize
			
			; Resize parameters base & change the button title:
			(*pstate).wLDMbaseYsize = ([35,430,330])[methodindex+1]
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
			WIDGET_CONTROL, (*pstate).wLDMbase, SCR_YSIZE=(*pstate).wLDMbaseYsize
			WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
			WIDGET_CONTROL, (*pstate).wLDMbttn, SET_VALUE='-', SET_UVALUE='+'
		END
		
		"LIMB DETECTION MORE BUTTON": BEGIN
			; Change the button title from "+" to "-" or vice cersa:
			WIDGET_CONTROL, event.ID, GET_VALUE=curVal, GET_UVALUE=newVal
			WIDGET_CONTROL, event.ID, SET_VALUE=newVal, SET_UVALUE=curVal
				
			; Resize parameters base:
			visible = curVal EQ '+'
			methodindex = WIDGET_INFO((*pstate).wLDdlist, /DROPLIST_SELECT)
			(*pstate).wLDMbaseYsize = ([35,430,330])[visible*(methodindex+1)]
			WIDGET_CONTROL, (*pstate).wLDMbase, SCR_YSIZE=(*pstate).wLDMbaseYsize
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
			WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
						
			FOR i=0,N_ELEMENTS((*pstate).wLDMsbases)-1 $
			DO WIDGET_CONTROL, (*pstate).wLDMsbases[i], MAP = i EQ methodindex ? 1 : 0
		
		END
		
		;***********************************************************************
		; Fit limb from the image with limb from the model:
		;***********************************************************************
		"LIMB FIT": BEGIN
			IF selSatIndex EQ !NULL THEN BREAK
			WIDGET_CONTROL, wSaveLbl, SET_VALUE=""
			WIDGET_CONTROL, /HOURGLASS
			IF N_ELEMENTS(satModel) EQ 0 || N_ELEMENTS(satLimb) EQ 0 THEN BREAK
						
			WIDGET_CONTROL, event.ID, GET_UVALUE=wLFdlist
			methodindex = WIDGET_INFO(wLFdlist, /DROPLIST_SELECT)
			
			CASE methodindex OF
				0: caviar_satLimbFit_ite, satModel, satLimb, $
									TOLERANCE=(*pstate).fitThreshold, sres, lres
				1: caviar_satLimbFit_lsq, planets[selSatIndex], satModel, satLimb, $
									sres, lres
			ENDCASE
			dres = SQRT(sres*sres+lres*lres)
			
			setStruct, satModel, 'SLSIGMA', [[sres],[lres]]
			caviar_satPos_setResults, "LIMB-FIT", satModel.slCenter, dres
			caviar_satPos_gui_table, /UPDATE_VALUES
			caviar_display
		END
		
		"LIMB FIT THRESHOLD": (*pstate).fitThreshold = DOUBLE(event.str)
				
		ELSE:
	ENDCASE	
		
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATLIMBFIT_GUI
; PURPOSE: Display a graphical interface with:
;	- The list of satellites visible in the image and their data,
;	- A window to display the satellite image,
;	- Buttons to launch satellite center of figure mesurement routines and to 
;	save the results.
;		
; CATEGORY:
;       Widgets
;
; INPUTS:
;       None.
; KEYWORDS:
;		None.
; OUTPUTS:
;       None.
; COMMON BLOCKS:
;       
; SIDE EFFECTS:
;       A widget window is created.
; RESTRICTIONS:
;       
; MODIFICATION HISTORY:
;		2012, december			MEUNIER L-E		OBSPM>IMCCE
;			- Written
;		2013, april				MEUNIER L-E		OBSPM>IMCCE
;			- Change UI
;-------------------------------------------------------------------------------
PRO caviar_satlimbfit_gui, PARENT=wMAINbase, GROUP_LEADER=groupLeader, $
							XOFFSET=xoffset, YOFFSET=yoffset, TITLE=title, $
							FRAME=frame, XPAD=xpad, YPAD=ypad, SPACE=space
	
	COMMON CAVIAR_GUI, wCAVIARtlb, wSATLIMBFITbase
	COMMON CAVIAR_DATA, image
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
						

	; Test if widget window is already launched
	IF(XRegistered('caviar_satLimbFit_gui') NE 0) THEN RETURN

	IF N_ELEMENTS(image) EQ 0 THEN BEGIN
		msg = ["CAVIAR_SATLIMBFIT_GUI:", "Please load an image first!"]
		res = DIALOG_MESSAGE(msg, /CENTER)
		RETURN
	ENDIF
	
		
	;***************************************************************************
	; Set Parameters 
	;***************************************************************************
	; @孙德淦，2022.9.15,增加DSK模型选项
	SMlist = ["Spice Ellipsoid", "Digital Shape Model"]  
	nptsList = ['200', '500', '1000', '2000', '5000', '10000']
	nptsList_dfltID = 2
	satModelNpts = LONG(nptsList[nptsList_dfltID])
	
	; For Shape model:
	; smcp参数代表什么意思
	smcp = { VALUES: [3,1.0,0.4,0.6], FORMAT: ['(I-3)','(G0)','(G0)','(G0)'], $
			 MIN: [1,0.01,0.01,0.1], MAX: [30,1,1,3], STEP: [2,0.01,0.01,0.1], $
			 TEXT: ["Median Width  : ","High threshold: ","Low threshold : ","Convol. sigma : "]}
	smcpVals = smcp.VALUES   ; 这个值代表什么意思？
	smAutoUpdt = 1
	
	; For Limb detection:
	ldmethods = ["Gradient", "Canny"]
	
	; For Limb fitting:
	LFlist = ["Iterative offset","Least squares"]
	fitThresholdList = ['0.5','1.0','2.0','4.0','6.0','8.0','10.0']
	fitThresholdList_dfltID = 2
	
	wSMPbaseYsize = 1
	wLDMbaseYsize = 35
	wSLFcplmtYsize = 225

	;*************************************************************************** 
	; Define the widget base and subwidgets tree 
	;***************************************************************************
	IF NOT KEYWORD_SET(title) THEN title = "Fit satellite limb"
	
	
	IF KEYWORD_SET(wMAINbase) THEN BEGIN
		IF NOT WIDGET_INFO(wMAINbase, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid widget identifier.',/ERROR)
	ENDIF ELSE BEGIN
		IF NOT KEYWORD_SET(groupLeader) $
		THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
										UNAME="CAVIAR_SATLIMBFIT_GUI") $
		ELSE BEGIN
			IF WIDGET_INFO(groupLeader, /VALID_ID) $
			THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
										UNAME="CAVIAR_SATLIMBFIT_GUI", $
										GROUP_LEADER=groupLeader, /FLOATING) $
			ELSE res=DIALOG_MESSAGE('Invalid group leader widget identifier.',/ERROR)
		ENDELSE
	ENDELSE
	wSATLIMBFITbase = wMAINbase
	
	;***************************************************************************
	; Define subwidgets tree:
	;***************************************************************************
	; extra组件是第二大组件
	extra = WIDGET_BASE(wMAINbase, /COLUMN, XPAD=xpad, YPAD=ypad)
	
	wbase = WIDGET_BASE(extra, /COLUMN, FRAME=frame, XPAD=0, YPAD=0, SPACE=space, $
									/BASE_ALIGN_CENTER, /ALIGN_CENTER)
		
		wLDbase  = WIDGET_BASE(wbase, /COL, XPAD=0, YPAD=0, SPACE=0)
			wLDsbase1 = WIDGET_BASE(wLDbase, /ROW, /BASE_ALIGN_CENTER, XPAD=0, YPAD=0, SPACE=0)
				wLDdlist = WIDGET_DROPLIST(wLDsbase1, UNAME="LIMB DETECTION METHOD", $
									VALUE=ldmethods, TITLE="Limb detection method:")
				wSpace = WIDGET_BASE(wLDsbase1, XSIZE=32)
				wLDMbttn = WIDGET_BUTTON(wLDsbase1, UNAME="LIMB DETECTION MORE BUTTON", $
								VALUE="+", UVALUE="-", XSIZE=29, YSIZE=29)
				WIDGET_CONTROL, wLDdlist, SET_UVALUE=wLDMbttn
			wLDsbase2 = WIDGET_BASE(wLDbase)
			
		wSpace = WIDGET_BASE(wbase, XSIZE=300, YSIZE=1, /FRAME)
			
		; 边缘检测及模型选择、边缘拟合基础组件
		wSMbase  = WIDGET_BASE(wbase, /COL, XPAD=0, YPAD=0, SPACE=0)
		    ; Shape Model基础组件
			wSMsbase1 = WIDGET_BASE(wSMbase, /ROW, /BASE_ALIGN_CENTER, XPAD=0, YPAD=0, SPACE=0)
			  ; 模型选择下拉菜单组件
				wSMdlist = WIDGET_DROPLIST(wSMsbase1, UNAME="SHAPE MODEL SELECT", $
									VALUE=SMlist, TITLE="Shape model:")
				wSpace = WIDGET_BASE(wSMsbase1, XSIZE=16)
				; 设置模型边缘点数量按钮，和模型参数组件相关联
				wSMPARAMSbttn = WIDGET_BUTTON(wSMsbase1, UNAME="SHAPE MODEL PARAMS", $
								VALUE="+", UVALUE="-", XSIZE=29, YSIZE=29)
				WIDGET_CONTROL, wSMdlist, SET_UVALUE=wSMPARAMSbttn
			
			; 自动更新组件，和模型加载组件、模型移动组件在同一组件内
			wSMsbase2 = WIDGET_BASE(wSMbase, /ROW, /BASE_ALIGN_CENTER, /ALIGN_CENTER)
				wSMUPDTBttn = CW_BGROUP(wSMsbase2, "Auto-update", $
									UNAME='SM AUTO UPDATE', SET_VALUE=smAutoUpdt, /NONEXCLUSIVE)
				wSpace = WIDGET_BASE(wSMsbase2, XSIZE=77)
        ; 模型加载按钮,点击加载模型，宽47个像素，高29个像素
				wSMLOADbttn = WIDGET_BUTTON(wSMsbase2, UNAME="SHAPE MODEL LOAD", $
									VALUE="Load", XSIZE=47, YSIZE=29)
				; 模型移动按钮，点击出现界面，宽47个像素，高29个像素
				wSMMOVEbttn = WIDGET_BUTTON(wSMsbase2, UNAME="SHAPE MODEL MOVE", $
									VALUE="Move", XSIZE=47, YSIZE=29)
				wSpace = WIDGET_BASE(wSMsbase2, XSIZE=1)
				
			; 模型参数组件
			wSMPbase = WIDGET_BASE(wSMbase, SCR_YSIZE=wSMPbaseYsize)
		wSpace = WIDGET_BASE(wbase, XSIZE=300, YSIZE=1, /FRAME)
		wLFbase = WIDGET_BASE(wbase, /COL, SPACE=0, /BASE_ALIGN_LEFT, XSIZE=300)
	
	; Define SHAPE MODEL PARAMETERS widgets:
	; 定义模型参数组件
	wSMPARAMSbases = LONARR(3) ; 32位有符号长整数数组，用来存储组件ID
	; 遍历数组
	FOR i=0,N_ELEMENTS(wSMPARAMSbases)-1 $
	 ; ALIGN_CENTER:设置此关键字以将新小部件与其父基的中心对齐。 
	 ; 要生效，父级必须是 ROW 或 COLUMN 基数。 
	 ; 在 ROW 基础中，新的小部件将垂直居中。 
	 ; 在 COLUMN 基础中，新小部件将水平居中。
	 ; COL:如果包含此关键字，则base组件将其子组件布置在列中。 此关键字的值指定要使用的列数。 
	 ; 每列中子小部件的数量是通过在列中尽可能均匀地分布子小部件来确定的。 
	 ; 同时指定 COLUMN 和 ROW 关键字会导致错误。
	 ; ROW:子组件行数。
	 ; SCR_XSIZE:将此关键字设置为所需的小部件“屏幕”宽度(默认为像素）。 
	 ; 在很多情况下，设置此关键字与设置 XSIZE 关键字相同。
	 ; XPAD, YPAD:子组件和其父组件X，Y方向边缘间隔
	 ; SPACE：以像素指定行或列主要基础组件的子组件之间的空间。 如果存在 EXCLUSIVE 或 NONEXCLUSIVE 关键字，则忽略此关键字。
	 ; MAP: 一旦实现了一个子部件层次结构，它就可以被映射（可见）或未映射（不可见）。 
	 ; 此关键字指定给定基础组件及其后代的初始映射状态。 指定非零值表示基础组件在显示时应该被映射（默认值）。 
	 ; 零值表示基础组件在初始时应该取消映射。 在基础组件显示之后，可以使用MAP关键字将其映射状态更改为WIDGET_CONTROL过程。
	DO wSMPARAMSbases[i] = WIDGET_BASE(wSMPbase, /COL, SCR_XSIZE=300, /ALIGN_CENTER, $
								XPAD=0, YPAD=0, SPACE=0, MAP= i EQ 0 ? 1 : 0)
								
    ; 三维椭球模型参数组件
		wSpiceEllParamsBase = wSMPARAMSbases[0]
			wEllNpts_Base  = WIDGET_BASE(wSpiceEllParamsBase, /ROW)
				wEllNpts_Lbl  = WIDGET_LABEL(wEllNpts_Base, VALUE="Number of points:")
				wEllNpts_dlist = WIDGET_COMBOBOX(wEllNpts_Base, /EDITABLE, /FLAT, $
								XSIZE=80, VALUE=nptsList, UNAME="SM ELLIPSOID NPOINTS")
			WIDGET_CONTROL, wEllNpts_dlist, SET_COMBOBOX_SELECT=nptsList_dfltID
     
    ; @孙德淦,2022.9.20
    ; 增加DSK模型边缘点数量参数选择组件
    wDSKParamsBase = wSMPARAMSbases[1]
      wDSKNpts_Base  = WIDGET_BASE(wDSKParamsBase, /ROW)
        wDSKNpts_Lbl  = WIDGET_LABEL(wDSKNpts_Base, VALUE="Number of points:")
        wDSKNpts_dlist = WIDGET_COMBOBOX(wDSKNpts_Base, /EDITABLE, /FLAT, $
      XSIZE=80, VALUE=nptsList, UNAME="SM DSK NPOINTS")
    WIDGET_CONTROL, wDSKNpts_dlist, SET_COMBOBOX_SELECT=nptsList_dfltID
    
;		wSMmove = CW_MOVE(wSMPARAMSbases[2], UNAME="MOVE", /ROW)
	
	; Define LIMB DETECTION PARAMETERS widgets:
	wLDMsbases = LONARR(N_ELEMENTS(ldmethods))
	FOR i=0,N_ELEMENTS(ldmethods)-1 $
	DO wLDMsbases[i] = WIDGET_BASE(wLDsbase2, /COL, /ALIGN_CENTER, $
						XPAD=0, YPAD=0, SPACE=0, MAP= i EQ 0 ? 1 : 0)
		
	; Define LIMB FITTING widgets:							
	wLFlbl = WIDGET_LABEL(wLFbase, VALUE="Fit SHAPE MODEL to DETECTED LIMB", /ALIGN_CENTER)
	wLFsbase = WIDGET_BASE(wLFbase, /ROW, /ALIGN_LEFT, /BASE_ALIGN_CENTER)
		wLFdlist = WIDGET_DROPLIST(wLFsbase, UNAME="LIMB FIT METHOD", $
								VALUE=LFlist, TITLE="Method:")
		wLFbttn = WIDGET_BUTTON(wLFsbase, UNAME="LIMB FIT", VALUE="Fit", $
			UVALUE=wLFdlist, XSIZE=40, YSIZE=29)
	wLFThreshBase = WIDGET_BASE(wLFbase, /ROW, XPAD=0, YPAD=0, SPACE=0)
		wFitThresLbl  = WIDGET_LABEL(wLFThreshBase, VALUE=" Threshold (pixel):")
		wFitThresdlist = WIDGET_COMBOBOX(wLFThreshBase, /EDITABLE, XSIZE=70, $
						VALUE=fitThresholdList, UNAME="LIMB FIT THRESHOLD")
		WIDGET_CONTROL, wFitThresdlist, SET_COMBOBOX_SELECT=fitThresholdList_dfltID
		
	;***************************************************************************
	; Realize main base, control subwidgets and initialize:
	;***************************************************************************
	WIDGET_CONTROL, wMAINbase, /REALIZE, UPDATE=0
	caviar_xmove, image.window, PROC2CALL_NAME='caviar_satModel_move', $
						PARENT=wSMPARAMSbases[2], FRAME=0
	caviar_edgrad_gui, PARENT=wLDMsbases[0]
	caviar_edcanny_gui, PARENT=wLDMsbases[1]
	WIDGET_CONTROL, wMAINbase, UPDATE=1
	WIDGET_CONTROL, wLDsbase2, SCR_YSIZE=wLDMbaseYsize
	
	caviar_satLimbFit_gui_init
	
	wSLFbaseYsize = wSMPbaseYsize + wLDMbaseYsize + wSLFcplmtYsize
	
	;***************************************************************************
	; Set 'state' structure with some parameters and copy it in 'extra' widget uvalue:
	; 设置state结构体参数，并且把它复制给extra组件的uvalue值，extra组件是边缘检测、模型选择和加载、边缘拟合的父组件
	;***************************************************************************
	state = {wSMdlist:wSMdlist, wSMPARAMSbttn:wSMPARAMSbttn, $
			 wSMPbase:wSMPbase, wSMPARAMSbases:wSMPARAMSbases, WSMPBASEYSIZE:wSMPbaseYsize, $
			 wLDdlist:wLDdlist, wLDMbttn:wLDMbttn, $
			 wLDMbase:wLDsbase2, WLDMBASEYSIZE:wLDMbaseYsize, wLDMsbases:wLDMsbases, $
			 WSLFCPLMTYSIZE:wSLFcplmtYsize, $
			 SATMODELNPTS:satModelNpts, SMAUTOUPDT:smAutoUpdt, SMCPVALS:smcpVals, $
			 FITTHRESHOLD:fitThresholdList[fitThresholdList_dfltID]}
	WIDGET_CONTROL, extra, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	; 和caviar_satLimbFit_gui_event响应函数相关联
	XMANAGER, 'caviar_satLimbFit_gui', wMAINbase, /JUST_REG
END
