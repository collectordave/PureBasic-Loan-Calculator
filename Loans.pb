;{ ==Code Header Comment==============================
;        Name/title: Loans.pb
;   Executable name: 
;           Version: 1.0.1
;            Author: netmaestro
;       Modyfied By: collectordave
;    Translation by: 
;       Create date: N/A
; Previous releases: 
; This Release Date: 
;  Operating system: Windows  [X]GUI
;  Compiler version: PureBasic 5.6B2 (x64)
;         Copyright: (C)2016
;           License: Credit Only
;         Libraries: 
;     English Forum: 
;      French Forum: 
;      German Forum: 
;  Tested platforms: Windows
;       Description: Calculates Loan Interest ETC
; ====================================================
;.......10........20........30........40........50........60........70........80
;}

IncludeFile "CDPrint.pbi"

Enumeration FormWindow
  #WinMain
  #txtAmount
  #strAmount
  #txtInterest
  #strInterest
  #txtPercent
  #txtPeriod
  #strPeriod
  #txtYears
  #txtPayments
  #strPayments
  #txtComponds
  #strCompounds
  #btnCalculate
  #listAmort
  #txtMinPayment
  #strMinimumPayment
  #txtNewPayment
  #strNewPayment
  #btnReCalculate  
  #txtTotalPayments
  #strTotalPayments
  #txtTotalInterest
  #strTotalInterest
  #txtTotalPaid
  #strTotalPaid
  #txtFinalPayment
  #strFinalPayment  
  #mnuAmortization
  #mnuPrint
EndEnumeration

Global LoanAmount.d,Interest.d,PayAmount.d
Global Years.l,Payments.l,Compounds.l
Global TotalInterest.d,TotalPaid.d,FinalPayment.d

Procedure Calculate()
  
  LoanAmount = ValD(GetGadgetText(#strAmount))
  Interest = ValD(GetGadgetText(#strInterest))/100  
  Years = Val(GetGadgetText(#strPeriod)) 
  Payments = Val(GetGadgetText(#strPayments)) 
  Compounds = Val(GetGadgetText(#strCompounds)) 
  
  ;Calculate Minimum Payment
  PayAmount =  LoanAmount * (Pow(1 + Interest / Compounds , Compounds / Payments ) - 1) / (1 - Pow( 1 + Interest / Compounds , - Years * Compounds))
  
  SetGadgetText(#strMinimumPayment,StrD(PayAmount,2))  
  
EndProcedure

Procedure ReCalculate(PayAmount.d)
  
  Define NumberOfPayments.i,iLoop.i,CompInterest.d,Primary.d
  
  LoanAmount = ValD(GetGadgetText(#strAmount))
  
  
  NumberOfPayments = Years * Payments
    
  Dim LoanSchedule.s(NumberOfPayments + 1 , 5)
  
  Define iLoop.i
  iLoop = 0
  While LoanAmount > 1

    iLoop + 1
    CompInterest = LoanAmount * (Pow(1+Interest/Compounds, Compounds/Payments) - 1)
    Primary = PayAmount - CompInterest
    LoanAmount - Primary
    If PayAmount < LoanAmount
      LoanSchedule(iLoop,0) = RSet(Str(iLoop)+":",5)
      LoanSchedule(iLoop,1) = RSet(StrD(PayAmount,2),7)
      LoanSchedule(iLoop,2) = RSet(StrD(CompInterest,2),9)
      LoanSchedule(iLoop,3) = RSet(StrD(Primary,2),9)
      LoanSchedule(iLoop,4) = RSet(StrD(Abs(LoanAmount),2),10)
    Else
      LoanAmount=LoanAmount+LoanAmount*Interest/12
      If Abs(LoanAmount-PayAmount)<1
        LoanAmount=PayAmount
      EndIf
      LoanSchedule(iLoop,0) = RSet(Str(iLoop)+":",5)
      LoanSchedule(iLoop,1) = RSet(StrD(PayAmount,2),7)
      LoanSchedule(iLoop,2) = RSet(StrD(CompInterest,2),9)
      LoanSchedule(iLoop,3) = RSet(StrD(Primary,2),9)
      LoanSchedule(iLoop,4) = RSet(StrD(Abs(LoanAmount),2),10)
      CompInterest = LoanAmount*Interest/12
      Primary = LoanAmount - CompInterest
      LoanAmount = 0
      iLoop + 1
      LoanSchedule(i,0) = RSet(Str(iLoop)+":",5)
      LoanSchedule(iLoop,1) = RSet(Trim(LoanSchedule(iLoop - 1,4)),7)
      LoanSchedule(iLoop,2) = RSet(StrD(CompInterest,2),9)
      LoanSchedule(iLoop,3) = RSet(StrD(Primary,2),9)
      LoanSchedule(iLoop,4) = RSet(StrD(Abs(LoanAmount),2),10)
    EndIf
  Wend 
  
  iLoop=1
  
  ClearGadgetItems(#listAmort)
  TotalInterest = 0
  TotalPaid = 0
  While LoanSchedule(iLoop,0)<>""
    out$=LoanSchedule(iLoop,0)+LoanSchedule(iLoop,1)+Chr(10)+LoanSchedule(iLoop,2)+Chr(10)+LoanSchedule(iLoop,3)+Chr(10)+LoanSchedule(iLoop,4)
    AddGadgetItem(#listAmort,-1,out$)
    TotalPaid = TotalPaid + ValD(LoanSchedule(iLoop,1))    
    TotalInterest = TotalInterest + ValD(LoanSchedule(iLoop,2))   
    FinalPayment = ValD(LoanSchedule(iLoop,4))  
    iLoop+1
  Wend
  
  SetGadgetText(#strTotalInterest, StrD(TotalInterest,2))
  SetGadgetText(#strTotalPaid, StrD(TotalPaid + FinalPayment,2)) 
  SetGadgetText(#strTotalPayments,Str(iLoop - 1))
  SetGadgetText(#strFinalPayment, StrD(FinalPayment,2))  
  
EndProcedure

Procedure PrintAmortization()
  
  Define PageHeight.i,PageWidth.i,Currentx.i,Currenty.i,Orientation.i,RowHeight.i,GridWidth.i
  Define PrintText.s
  Define iLoop.i,jLoop.i
  
  ;Open The Print Job
  CDPrint::Open("Loan Calculator",CDPrint::#Preview)
  PageHeight = CDPrint::Printer\Height - (CDPrint::Printer\TopPrinterMargin * 2) ;Useable Height
  PageWidth = CDPrint::Printer\Width - (CDPrint::Printer\LeftPrinterMargin * 2) ;Useable Width
      
  If PageHeight > PageWidth
    Orientation = CDPrint::#Portrait
  Else
    Orientation = CDPrint::#Landscape
  EndIf
      
  ;Add First Page
  CDPrint::AddPage(Orientation)     
  Currentx = 0 
  Currenty = 0
  
  ;Font Used is Arial 14
  RowHeight = (14 * 0.352777778) + 2            ;TextHeight in mm + 2mm
  
  GridWidth = 40 * 4 ;Sum Of Columnwidths
  
  ;First Page Column Headers
  CDPrint::PrintLine(0,0,GridWidth,0,0.1,RGBA(128,128,128,255))
  ;For Each Column
  For iLoop = 0 To 3
    CDPrint::PrintLine(Currentx + (iLoop * 40), 0,Currentx + (iLoop * 40),RowHeight,0.1,RGBA(128,128,128,255))  
    CDPrint::PrintLine(Currentx + ((iLoop + 1) * 40), 0,Currentx + ((iLoop + 1) * 40),RowHeight,0.1,RGBA(128,128,128,255))    
    PrintText = GetGadgetItemText(#listAmort, -1, iLoop)
    CDPrint::PrintText(Currentx + (iLoop * 40) + 1,1,"Arial",14,PrintText)
  Next iLoop
  
  jLoop = 0
  
  While jLoop <  CountGadgetItems(#listAmort) 
    
    If (Currenty + (RowHeight * 2)) => PageHeight
      
      ;Add Bottom Line For Grid
      CDPrint::PrintLine(0, Currenty + RowHeight,GridWidth,Currenty + RowHeight,0.1,RGBA(128,128,128,255))  
      
      ;New Page Needed
      CDPrint::AddPage(Orientation)
      
      ;Column Headers
      CDPrint::PrintLine(0,0,GridWidth,0,0.1,RGBA(128,128,128,255))
      ;For Each Column
      For iLoop = 0 To 3
        ;Vertical Grid Lines       
        CDPrint::PrintLine(Currentx + (iLoop * 40), 0,Currentx + (iLoop * 40),RowHeight,0.1,RGBA(128,128,128,255))  
        CDPrint::PrintLine(Currentx + ((iLoop + 1) * 40), 0,Currentx + ((iLoop + 1) * 40),RowHeight,0.1,RGBA(128,128,128,255))         
        ;Get The Text
        PrintText = Trim(GetGadgetItemText(#listAmort, -1, iLoop))
        ;Print the Text
        CDPrint::PrintText(Currentx + (iLoop * 40) + 1,1,"Arial",14,PrintText)
      Next iLoop
      
      ;Set CurrentY
      Currenty = RowHeight
      
    Else
      Currenty = Currenty + RowHeight
    EndIf
    
    CDPrint::PrintLine(0,Currenty,GridWidth,Currenty,0.1,RGBA(128,128,128,255))
    
    ;For Each Column
    For iLoop = 0 To 3
      ;Vertical Grid Lines
      CDPrint::PrintLine(Currentx + (iLoop * 40), Currenty,Currentx + (iLoop * 40),Currenty + RowHeight,0.1,RGBA(128,128,128,255))  
      CDPrint::PrintLine(Currentx + ((iLoop + 1) * 40), Currenty,Currentx + ((iLoop + 1) * 40),Currenty + RowHeight,0.1,RGBA(128,128,128,255))      
      PrintText = GetGadgetItemText(#listAmort, jLoop, iLoop)
      CDPrint::PrintText(Currentx + (iLoop * 40) + 1,Currenty + 1,"Arial",14,PrintText)
    Next iLoop
    
    ;Next Row
    jLoop = jLoop + 1


  Wend
  ;Add Bottom Line For Grid
  CDPrint::PrintLine(0, Currenty + RowHeight,GridWidth,Currenty + RowHeight,0.1,RGBA(128,128,128,255))  
  CDPrint::Finished()
  
  
  
 ; For jLoop = -1 To CountGadgetItems(#listAmort)
 ; For iLoop = 0 To 3
 ;   Debug GetGadgetItemText(#listAmort, jLoop, iLoop)
    ;Print each column
 ; Next iLoop
 ; Next jLoop
  
EndProcedure

Procedure Schedule()
  
  Define NumberOfPayments.i,iLoop.i,CompInterest.d,Primary.d
  
  NumberOfPayments = Years * Payments
    
  Dim LoanSchedule.s(NumberOfPayments + 1 , 5)
  
  iLoop = 0
  While LoanAmount>1
    iLoop + 1
    CompInterest = LoanAmount * (Pow(1 + Interest / Compounds , Compounds /Payments) - 1)
    Primary = PayAmount - CompInterest
    LoanAmount - Primary
    LoanSchedule(iLoop,0) = RSet(Str(iLoop)+":",5)
    LoanSchedule(iLoop,1) = RSet(StrD(PayAmount,2),7)
    LoanSchedule(iLoop,2) = RSet(StrD(CompInterest,2),9)
    LoanSchedule(iLoop,3) = RSet(StrD(Primary,2),9)
    LoanSchedule(iLoop,4) = RSet(StrD(Abs(LoanAmount),2),10)
  Wend
  
  iLoop=1
  TotalInterest = 0
  TotalPaid = 0
  FinalPayment = 0
  ClearGadgetItems(#listAmort)
  While LoanSchedule(iLoop,0)<>""
    out$=LoanSchedule(iLoop,0)+LoanSchedule(iLoop,1)+Chr(10)+LoanSchedule(iLoop,2)+Chr(10)+LoanSchedule(iLoop,3)+Chr(10)+LoanSchedule(iLoop,4)
    TotalPaid = TotalPaid + ValD(LoanSchedule(iLoop,1))
    TotalInterest = TotalInterest + ValD(LoanSchedule(iLoop,2))
    AddGadgetItem(#listAmort,-1,out$)
    FinalPayment = ValD(LoanSchedule(iLoop,4)) 
    iLoop+1
  Wend

  SetGadgetText(#strTotalInterest, StrD(TotalInterest,2))
  SetGadgetText(#strTotalPaid, StrD(TotalPaid + FinalPayment,2))   
  SetGadgetText(#strTotalPayments,Str(iLoop - 1))
  SetGadgetText(#strFinalPayment, StrD(FinalPayment,2))  
  
EndProcedure

OpenWindow(#WinMain, 5, 5, 750, 600, "Loan Calculator", #PB_Window_SystemMenu)
CreateMenu(0, WindowID(Window_0))
MenuTitle("Amortization")
MenuItem(#mnuPrint, "Print")
TextGadget(#txtAmount, 10, 10, 110, 20, "Amount", #PB_Text_Right)
StringGadget(#strAmount, 130, 10, 100, 20, "1000")
TextGadget(#txtInterest, 10, 40, 110, 20, "Interest", #PB_Text_Right)
StringGadget(#strInterest, 130, 40, 100, 20, "2")
TextGadget(#txtPercent, 240, 40, 20, 20, "%")
TextGadget(#txtPeriod, 10, 70, 110, 20, "Period", #PB_Text_Right)
StringGadget(#strPeriod, 130, 70, 100, 20, "10")
TextGadget(#txtYears, 240, 70, 40, 20, "Years")
TextGadget(#txtPayments, 10, 100, 110, 20, "Payments/Year", #PB_Text_Right)
StringGadget(#strPayments, 130, 100, 100, 20, "12")
TextGadget(#txtComponds, 10, 130, 110, 20, "Compounds/Year", #PB_Text_Right)
StringGadget(#strCompounds, 130, 130, 100, 20, "12")
ButtonGadget(#btnCalculate, 150, 160, 80, 30, "Calculate")
ListIconGadget(#listAmort,290,20,445,570,"Payment",120,#PB_ListIcon_GridLines|#PB_ListIcon_MultiSelect|#PB_ListIcon_FullRowSelect)
AddGadgetColumn(#listAmort,1,"Interest",100)
AddGadgetColumn(#listAmort,2,"Principal",100)
AddGadgetColumn(#listAmort,3,"Remaining",100)
LoadFont(0,"Courier New",10,#PB_Font_HighQuality)
SetGadgetFont(#listAmort,FontID(0))
TextGadget(#txtMinPayment, 10, 200, 110, 20, "Minimum Payment", #PB_Text_Right)
StringGadget(#strMinimumPayment, 130, 200, 100, 20, "")
TextGadget(#txtNewPayment, 10, 230, 110, 20, "Adjust Payment", #PB_Text_Right)
StringGadget(#strNewPayment, 130, 230, 100, 20, "") 
ButtonGadget(#btnReCalculate, 150, 260, 80, 30, "Re-Calculate") 
TextGadget(#txtTotalPayments, 20, 300, 110, 20, "Total Payments", #PB_Text_Right)
StringGadget(#strTotalPayments, 140, 300, 100, 20, "")
TextGadget(#txtTotalInterest, 20, 330, 110, 20, "Total Interest", #PB_Text_Right)
StringGadget(#strTotalInterest, 140, 330, 100, 20, "")
TextGadget(#txtTotalPaid, 20, 360, 110, 20, "Total Paid", #PB_Text_Right)
StringGadget(#strTotalPaid, 140, 360, 100, 20, "")
TextGadget(#txtFinalPayment, 20, 390, 110, 20, "Final Payment", #PB_Text_Right)
StringGadget(#strFinalPayment, 140, 390, 100, 20, "") 

Repeat
      
  Event = WaitWindowEvent()
    Select Event
      Case #PB_Event_CloseWindow
        End
  
    Case #PB_Event_Menu
      Select EventMenu()
          
        Case #mnuPrint
          
          PrintAmortization()
          
      EndSelect
  
    Case #PB_Event_Gadget
      Select EventGadget()
          
        Case #btnCalculate
            
          Calculate()
          Schedule()
            
        Case #btnReCalculate
            
          If ValD(GetGadgetText(#strNewPayment)) > PayAmount
            ReCalculate(ValD(GetGadgetText(#strNewPayment)))
          EndIf
            
      EndSelect
  EndSelect
    
ForEver
; IDE Options = PureBasic 5.60 Beta 1 (Windows - x64)
; CursorPosition = 13
; Folding = p
; EnableXP