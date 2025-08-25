'##############################################################################
'
' LTC - SERIAL DRIVER FOR LEDTRIKS        (c) Copyright Tim Wells
'
' VERSION 0.11    LTC_11.BAS         5/9/2010
'
'2025/08/07 (Aurbo99) code cleaning, removed PixC program routine
'
'2025/07/24 (LabRat) code clean-up (and network streaming?)
'
'05/09/10 Moved single panel clock to panel 1
'
'Added PixC support
'
'07/8/10 Added auto christmas detection
'
'        Added single panel clock
'
'  This program is distributed in the hope that it will be useful,
'  but WITHOUT ANY WARRANTY; without even the implied warranty of
'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
'  This program is provided free for you to use in any way that you wish,
'  subject to the laws and regulations where you are using it.  Due diligence
'  is strongly suggested before using this code.
'
'  The Author makes no warranty of any kind, express or implied, with regard
'  to this program or the documentation contained in this document.  The
'  Author shall not be liable in any event for incidental or consequential
'  damages in connection with, or arising out of, the furnishing, performance
'  or use of these programs.
' It is released to assist in explaining the concept only and may not work
' in all configs.
'
'##############################################################################

#include "vbcompat.bi"
#include "win/winsock2.bi"

'Constants for Winsock initialization
CONST WS_VERSION_REQUEST = &H0202 'Winsock 2.2

' -----------------
' var TIME related:
' -----------------
DIM as double currentTimeDate,dateDec25
DIM as string strDaysRemaining,strTimeRemaining,strTime
DIM as integer d_h,d_t,d_u,H_t,H_u,M_t,M_u,s_t,S_U,start,currentYear
DIM as integer previousS_U

' Look uptil actuatime > gTimeout
DIM as double gActualTime,gTimeout

currentYear=year(now)
dateDec25=dateserial(currentYear,12,25)

' --------------------
' FrameBuffer BitMap
' --------------------
DIM as ubyte Panel_1(767),Panel_2(767)
DIM as integer digits(9,34),clockDigits(10,76)

' --------------------
' FrameBuffers ByteMap
' --------------------
DIM Shared as ubyte panel1(95)
DIM Shared as ubyte panel2(95)
DIM Shared as ubyte panel3(95)
DIM Shared as ubyte panel4(95)

' ------------
' File access
' ------------
DIM as integer infile,outfile,scriptfile
DIM as string  inputfile,scriptfilename,file_id

' ----------------
' Com Port access
' ----------------
DIM Shared as integer comport
DIM as string sComOpenCmd,theport, theport1, strComPort

' ---------------------
' Command Line Parsing
' ---------------------
DIM as integer argc, parameters
DIM as string argv,cmd

' ------------------
' Winsock Variables
' ------------------
DIM wsaData AS WSADATA
DIM Shared sock AS SOCKET

' Panel Selection
DIM as integer p_pan,p_pan1
Dim as string p_select,p_select1

' Repeat count
DIM as integer repeatCount

' Decision y/n
DIM as integer decision

' Config parameters
DIM as integer panelCount
DIM as integer framedelay,hedgedelay,vedgedelay
DIM Shared as integer rotdelay


DIM as integer pixel,pixel1,pixelbyte,pixelpos,rows,cols
DIM Shared as integer diagFlag
DIM as integer p1byte1,p1byte2,p2byte1,p2byte2,p3byte1,p3byte2,p4byte1,p4byte2
DIM as integer a1,a2,a3,a4,b1,b2,b3,b4,sflag,pflag,testlen
DIM as integer yl,yr,use_script
DIM as integer X,Y,Z
DIM as integer rowCount, columnCount, bitCount, byteCount
DIM Shared as integer delay
DIM as integer nocommand,file_is_open

DIM as ubyte byte1
DIM as integer direction
DIM Shared as integer Rto,Rinc, RXC, RYC
DIM as string RD,B64quad,rextra,strDelay
DIM Shared as string content
DIM as double starttime
DIM Shared as double lastmark, tdelayms

CONST PI AS DOUBLE = 3.1415926535897932
CONST B64decode AS STRING ="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

file_is_open=0
diagFlag=0
previousS_U=-1 'impossible in normal use (used to rate limite clock frame updates)

'##############################################################################
'SUBS
'##############################################################################
Declare Sub handleRotation(direction as integer)
Declare Sub printCmdHelp
Declare Sub testPorts

Function escapeEncode(outchar as integer) as string
   if (outchar >124 AND outchar <128) then
       return chr$(127)+chr$(outchar-78)
   endif
   return chr$(outchar)
end Function

Sub sendUdp()
   if (send(sock, content, len(content), 0) = SOCKET_ERROR) then
      Print("send() error")
      closesocket(sock)
      end 1
   endif
end sub

'##############################################################################
' INITITIALISE PARAMETERS AND OPEN COM PORT
'##############################################################################

' Can't use a sub routine due to use of the DATA command in
' the dataInit procedure.. so goto and "goto as a return"

'Connect to host
   dim sa as sockaddr_in
   sa.sin_port        = htons( 8082 )
   sa.sin_family      = AF_INET
   'sa.sin_addr.S_addr = inet_addr("192.168.42.102")
   sa.sin_addr.S_addr = inet_addr("192.168.4.1")

goto dataInit
start:

   IF WSAStartup(WS_VERSION_REQUEST, @wsaData)<>0 THEN
        Print "WSAStartup failed!"
        END
    ENDIF

delay=50
starttime=timer
lastmark=timer
repeatCount=0

'set the default rotation center
RXC=23
RYC=7
Rinc=15
Rto=360

'Create the UDP socket
sock = opensocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)
'sock = opensocket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
if sock = INVALID_SOCKET THEN
    Print "Socket creation faile!d"
    WSACleanup
    END
ENDIF


   if( connect( sock, cast( PSOCKADDR, @sa ), sizeof( sa ) ) = SOCKET_ERROR ) then
       Print "connect() error"
       closesocket( sock )
       end 1
   end if


parameters=freefile
open "triksc.dat" for input as #parameters
    If Err Then
        Print
        Print "FAILED to open TRIKSC.DAT"
        Print "Would you like to setup (y/n)? ";
        decision=getkey
        Print chr$(decision)
        if decision= asc("y") or decision=asc("Y") then
            close
            Goto setup
        else
            closesocket(sock)
            END
        endif
    Else
        Print "TRIKSC.DAT FOUND"
        line input #parameters,strComPort
        strComPort=rtrim(strComPort)
        line input #parameters,strDelay
        panelCount=val(ltrim(strDelay))
        line input #parameters,strDelay
        framedelay=val(ltrim(strDelay))
        line input #parameters,strDelay
        hedgedelay=val(ltrim(strDelay))
        line input #parameters,strDelay
        vedgedelay=val(ltrim(strDelay))
        line input #parameters,strDelay
        rotdelay=val(ltrim(strDelay))

    End If


'supress wildcards
Extern _CRT_glob Alias "_CRT_glob" As Integer
Dim Shared _CRT_glob As Integer = 0


'Print "exe name= "; Command( 0 )

sComOpenCmd=strComPort+" 57600,n,8,1,cs0,ds0,cd0,rs"

comport=freefile
Open Com sComOpenCmd As #comport

If Err Then
        Print "FAILED to open Comport!!! "; strComPort
        Print "### IS IT ALREADY OPEN BY ANOTHER COMMAND??? #####"
        sleep 5000
        END
Else
        Print "COM port OK (";strComPort;")"
        Print "F_delay=";framedelay;" H_delay=";hedgedelay
        Print "V_delay=";vedgedelay;" R_delay=";rotdelay
        Print "Number of Panels=";panelCount

End If

argc = 1
nocommand=1
use_script=0

'set the default rotation settings
RXC=23
RYC=7
Rinc=15
Rto=360

' Now process the commands, one by one

'##############################################################################
'Interpret next parameter
'##############################################################################

Next_Parameter:
    if use_script=1 then
        if eof(scriptfile) then
            close #scriptfile
            goto no_params
        else
            input #scriptfile,argv
        endif
    else
        argv = Command( argc )
    end if

    If( Len( argv ) = 0 ) Then
        If( argc = 1 ) Then
            printCmdHelp
        elseif (argc=2 and nocommand=1)then
            Print
            Print "Switching to script file : ";inputfile
            use_script=1
            scriptfile=freefile
            scriptfilename=inputfile
            Open scriptfilename for input as #scriptfile
            If Err Then
                Print "FAILED to open SCRIPT FILE !!!"
                sleep
                closesocket(sock)
                END
            Else
                Print "Script File Opened OK";
                goto next_parameter
            End If
         End If
         goto no_params
    End If
    argc += 1

    'now check to see if its a filename'
    if left(argv,1)<>"#"  then
        inputfile=argv
        goto Next_parameter
    else
        cmd=ucase(argv)
        Print "COMMAND:";cmd
        nocommand=0
    endif

    'now we have a command

'#############################################################################
'PROCESS COMMANDS that don't need file interpretation
'#############################################################################
if cmd ="#DIS" then
    diagFlag=1
    screenres 800,600,8
    goto Next_parameter

elseif cmd ="#CLS" then
    content = chr$(126)+chr$(158)+chr$(126)+chr$(166)+chr$(126)+chr$(198)
    Print #comport,, content
    sendUdp
    goto Next_parameter

elseif cmd="#BLK" then
    content = chr$(126)+chr$(152)
    Print #comport,, content
    sendUdp
    goto Next_parameter

elseif cmd="#SHW" then
    content = chr$(126)+chr$(159)
    Print #comport,, content
    sendUdp
    goto Next_parameter

elseif left(cmd,4) = "#DLY" then
    sleep val(mid(cmd,5))
    goto Next_parameter

elseif left(cmd,4) = "#FDL" then
    framedelay= val(mid(cmd,5))
    goto Next_parameter

elseif left(cmd,4) = "#VDL" then
    vedgedelay= val(mid(cmd,5))
    goto Next_parameter

elseif left(cmd,4) = "#HDL" then
    hedgedelay= val(mid(cmd,5))
    goto Next_parameter
elseif left(cmd,3) = "#NP" then
    panelCount= val(mid(cmd,4))
    goto Next_parameter

elseif left(cmd,4) = "#RDL" then
    rotdelay= val(mid(cmd,5))
    goto Next_parameter

elseif left(cmd,4) = "#RTO" then
    RTO= val(mid(cmd,5))
    goto Next_parameter

elseif left(cmd,5) = "#RINC" then
    Rinc= val(mid(cmd,6))
    goto Next_parameter

elseif left(cmd,4) = "#STA" then
    content = chr$(126)+chr$(129)
    Print #comport,,content
    sendUdp
    goto Next_parameter

elseif left(cmd,4) = "#TSD" then
    file_id = mid(cmd,5,2)
    content = chr$(126)+chr$(130)+file_id
    Print #comport,,content
    sendUdp
    Print "file ID:";file_id
    goto Next_parameter

elseif cmd="#TIME" then
     Print using "Time elapsed = #####.### seconds";timer-starttime
     Print "PRESS ANY KEY TO CONTINUE"
     sleep
     goto Next_parameter

elseif left(cmd,4)="#RPT" then
     if repeatCount< val(mid(cmd,5)) then
        repeatCount=repeatCount+1
        argc=1
        if use_script=1 then
         close #scriptfile
         scriptfile=freefile
         Open scriptfilename for input as #scriptfile
            If Err Then
                Print "FAILED to open SCRIPT FILE !!!"
                closesocket(sock)
                sleep
                END
            Else
                Print "Script File ReOpened OK"
            End If
        endif
    endif
    goto Next_parameter

elseif left(cmd,5)="#LOOP" then
    if inkey$="" then
        argc=1
        if use_script=1 then
         close #scriptfile
         scriptfile=freefile
         Open scriptfilename for input as #scriptfile
            If Err Then
                Print "FAILED to open SCRIPT FILE !!!"
                closesocket(sock)
                sleep
                END
            Else
                Print "Script File ReOpened OK"
            End If
        endif
    endif
    goto Next_Parameter

elseif left(cmd,4)="#RYC" then
    RYC=val(mid(cmd,5))
    goto Next_parameter

elseif Left(cmd,4)="#RXC" then
    RXC=val(mid(cmd,5))
    goto Next_parameter

elseif Left(cmd,4)="#ROT" then
        if left(cmd,5)="#ROTL" then
            ' direction =-1
            handleRotation(-1)
        else
            ' direction =1
            handleRotation(1)
        endif
        goto Next_parameter


elseif left(cmd,4) = "#CLK" then
    gTimeout= timer+val(mid(cmd,5))
    content = chr$(126)+chr$(142) 'clear the screen panel 1
    Print #comport,,content
    sendUdp

    Erase Panel_1
    Panel_1(5*48+14)=1  ' display the colons
    Panel_1(6*48+14)=1
    Panel_1(10*48+14)=1
    Panel_1(11*48+14)=1
    Panel_1(5*48+15)=1
    Panel_1(6*48+15)=1
    Panel_1(10*48+15)=1
    Panel_1(11*48+15)=1

    Panel_1(5*48+31)=1
    Panel_1(6*48+31)=1
    Panel_1(10*48+31)=1
    Panel_1(11*48+31)=1
    Panel_1(5*48+32)=1
    Panel_1(6*48+32)=1
    Panel_1(10*48+32)=1
    Panel_1(11*48+32)=1

    Do
        strTime=format(now,"hh:mm:ss")
        ' Retrieve formatted time string, and parse H, M, D
        H_T=val(mid(strTime,1,1))
        if H_T =0 then
            H_T=10
        endif
        H_U=val(mid(strTime,2,1))
        M_T=val(mid(strTime,4,1))
        M_U=val(mid(strTime,5,1))
        S_T=val(mid(strTime,7,1))
        S_U=val(mid(strTime,8,1))

        if (S_U = previousS_U) then
           goto skipClockLoop
        endif
        Print "Time: ";strTime
        previousS_U = S_U
        '  Print d_H,d_t,d_u
        '  Print h_t+H_u+m_t+M_u+s_t+S_u

        ' hours_tens
        start=3*48+0
        bitCount=0
        for rowCount=0 to 10
            for columnCount=0 to 6
                Panel_1(3*48+(rowCount*48)+columnCount)=clockDigits(h_t,bitCount)
                Panel_1(3*48+7+(rowCount*48)+columnCount)=clockDigits(h_u,bitCount)
                Panel_1(3*48+17+(rowCount*48)+columnCount)=clockDigits(m_t,bitCount)
                Panel_1(3*48+24+(rowCount*48)+columnCount)=clockDigits(m_u,bitCount)
                Panel_1(3*48+34+(rowCount*48)+columnCount)=clockDigits(s_t,bitCount)
                Panel_1(3*48+41+(rowCount*48)+columnCount)=clockDigits(s_u,bitCount)
                ' LabRat: can drop the use of bitCount and use (rowCount*7)+columnCount
                bitCount=bitCount+1
            next columnCount
        next rowCount

        content = chr$(126)+chr$(141) 'send if out panel 1
        ' output stream
        'LabRat - reverse byte order
        for columnCount=95 to 0 step -1
            ' Bit manipulation: from the bit array build BYTE representing value in column of ROW0
            byte1=Panel_1(columnCount*8+0)+2*Panel_1(columnCount*8+1)+4*Panel_1(columnCount*8+2)+8*Panel_1(columnCount*8+3)+16*Panel_1(columnCount*8+4)+32*Panel_1(columnCount*8+5)+64*Panel_1(columnCount*8+6)+128*Panel_1(columnCount*8+7)
            ' Escape Encoding the byte if necessary
            content = content+escapeEncode(byte1)
        next columnCount
        Print #comport,,content
        sendUdp
skipClockLoop:
        gActualTime=timer
    Loop until  gActualTime > gTimeout
    goto Next_parameter


elseif left(cmd,4) = "#CAL" then
    gTimeout= timer+val(mid(cmd,5))
    content=chr$(126)+chr$(149)
    for byteCount= 95 to 0 step -1
        byte1=panel1(byteCount) ' Use the saved FRAME template
        ' Escape encoding
        content=content+ escapeEncode(byte1)
    next byteCount

    content=content+chr$(126)+chr$(141)
    for byteCount= 95 to 0 step -1
        byte1=panel2(byteCount)
        content=content+ escapeEncode(byte1)
    next byteCount

    put #comport,,content
    sendUdp

    Do
        currentTimeDate=now
        strDaysRemaining=str$(int(dateDec25)-int(currentTimeDate)-1)
        strTimeRemaining=format(dateDec25-currentTimeDate,"hh:mm:ss")
        if val(strDaysRemaining)=-1 then
            strDaysRemaining="000"
        endif
        Print "days:";strDaysRemaining
        Print"time:";strTimeRemaining

        d_h = int(val(strDaysRemaining)/100)

        d_t=val(mid(strDaysRemaining,len(strDaysRemaining)-1,1))
        d_u=val(mid(strDaysRemaining,len(strDaysRemaining),1))


        H_T=val(mid(strTimeRemaining,1,1))
        H_U=val(mid(strTimeRemaining,2,1))
        M_T=val(mid(strTimeRemaining,4,1))
        M_U=val(mid(strTimeRemaining,5,1))
        S_T=val(mid(strTimeRemaining,7,1))
        S_U=val(mid(strTimeRemaining,8,1))

        '  Print d_H,d_t,d_u
        '  Print h_t+H_u+m_t+M_u+s_t+S_u

        bitCount=0
        ' LabRat tweak - simplified code (combined multiple loops)
        for rowCount=0 to 6  ' rows
           for columnCount=0 to 4      ' clock digit columns
                ' Days (hundred::tens::units)
                Panel_2(9*48+3+(rowCount*48)+columnCount)=digits(d_h,bitCount)
                Panel_2(9*48+9+(rowCount*48)+columnCount)=digits(d_t,bitCount)
                Panel_2(9*48+15+(rowCount*48)+columnCount)=digits(d_u,bitCount)
                ' Hours (tens::units)
                Panel_2(9*48+30+(rowCount*48)+columnCount)=digits(h_t,bitCount)
                Panel_2(9*48+36+(rowCount*48)+columnCount)=digits(h_u,bitCount)
                ' Minutes (tens::units)
                Panel_1(9*48+3+(rowCount*48)+columnCount)=digits(m_t,bitCount)
                Panel_1(9*48+9+(rowCount*48)+columnCount)=digits(m_u,bitCount)
                ' Seconds (tens::units)
                Panel_1(9*48+27+(rowCount*48)+columnCount)=digits(s_t,bitCount)
                Panel_1(9*48+33+(rowCount*48)+columnCount)=digits(s_u,bitCount)
                bitCount = bitCount+1
            next columnCount
        next rowCount


        content = chr$(126)+chr$(149)
        ' Escape encoding the output stream (panel 1)
        for byteCount=95 to 0 step -1
            byte1=Panel_1(byteCount*8+0)+2*Panel_1(byteCount*8+1)+4*Panel_1(byteCount*8+2)+8*Panel_1(byteCount*8+3)+16*Panel_1(byteCount*8+4)+32*Panel_1(byteCount*8+5)+64*Panel_1(byteCount*8+6)+128*Panel_1(byteCount*8+7)
            content = content+escapeEncode(byte1)
        next byteCount
        Print #comport,, content
        sendUdp

        content = chr$(126)+chr$(141)
        for byteCount=95 to 0 step -1
            byte1=Panel_2(byteCount*8+0)+2*Panel_2(byteCount*8+1)+4*Panel_2(byteCount*8+2)+8*Panel_2(byteCount*8+3)+16*Panel_2(byteCount*8+4)+32*Panel_2(byteCount*8+5)+64*Panel_2(byteCount*8+6)+128*Panel_2(byteCount*8+7)
            content = content+escapeEncode(byte1)
        next byteCount
        Print #comport,, content
        sendUdp

        if h_t+H_u+m_t+M_u+s_t+S_u=0 then
            exit do
        endif
        ' check if timeout has occurred
        gActualTime=timer
    Loop until  gActualTime > gTimeout
    goto Next_parameter
endif

'##############################################################################
' Now  interpret a VIXEN LEDTRIKS FILE
'##############################################################################

Print "INTERPRETING VIXEN FILE : ";inputfile

infile=freefile
Open inputfile for input as #infile
    If Err Then
        Print "FAILED to open LEDTRIKS FILE !!!"
        Print "(Check the syntax of your command line)"
        closesocket(sock)
        sleep
        END
    Else
      Print "LEDTRIKS File Opened OK"
    End If

Find_Frames:
Input #infile,rd
if eof(infile) then goto byebye: end if

if instr(rd,"Frames")<>0 Then
    Print "Found Frame Data"
    Goto GetData
else
    Goto Find_Frames
end if

GetData:
input #infile,rd
'Print "RD:";rd
if eof(infile) then
    goto ByeBye
END IF

if instr(rd,"Frame length")<>0 then
    'process the next frame

    erase panel1
    erase panel2
    erase panel3
    erase panel4
   ' Print "line length=";len(rd)
   ' Print rd;":"

'Basically - the input command only seems to read 4kbytes. For big frames this
'isn;t enough... so need to read multiple lines and concatentate them
'empirically ..the first line of the the new read seems to have the last char of
'the last read... so for now just strip the first char without knowning why...


'keep concatenating until we have the end of frame
    while instr(rd,"/Frame")=0
        input #infile,rextra
 '       Print "xtra:";rextra
 '       sleep
        rd=rd+mid(rextra,2)
    wend


    Z=instr(rd,"=")+2
    'could look for frame delay
    z=instr(z,rd,">")+1

sflag=0
pflag=0
Panel_loop:
' timertest=timer

         b64quad=mid(rd,z,4)
'         Print b64quad;" ";

    if instr(b64quad,"<")<>0 then
        goto framedone
    endif
    ' Now just check if there's an ugly boundary we need to handle
    if len(b64quad)<>4 then
 '            Print "short";len(b64quad);":";b64quad;":"
        input #infile, rd
        z=1
        b64quad=mid(rd,z,4)
'       Print "done:" ;b64quad
        if instr(b64quad,"<")<>0 then
            goto framedone
        endif
    endif

    a1=instr(b64decode,mid(b64quad,1,1))-1
    a2=instr(b64decode,mid(b64quad,2,1))-1
    a3=instr(b64decode,mid(b64quad,3,1))-1
    a4=instr(b64decode,mid(b64quad,4,1))-1


    if a3<0 then
        a3=0
    endif
    if a4<0 then
        a4=0
    endif

    if sflag=0 then
        b1= ((a1 shl 2) and &b11111100) + ((a2 shr 4) and &b0000011)
        b2= ((a2 shl 4) and &b11110000) + ((a3 shr 2) and &b00001111)
        b3= ((a3 shl 6) and &b11000000) + (a4 and &b00111111)
        sflag=1
    elseif sflag=1 then
        b4= ((a1 shl 2) and &b11111100) + ((a2 shr 4) and &b0000011)
   '          Print b1;b2;b3;b4;"*";

           if b1<16 then
                if b3<48  then
                    'PANEL 1
                    pixel= b1*48 +b3
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel1(pixelbyte)=panel1(pixelbyte) or 2^pixelpos
                elseif (b3-48)<48  then
                    'PANEL 2
                    pixel= b1*48 +b3-48
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel2(pixelbyte)=Panel2(pixelbyte) or 2^pixelpos
                else
        '            Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
         '           Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"

                endif
            elseif b1<32 then
                if b3<48  then
                    'PANEL 3
                    pixel= (b1-16)*48 +b3
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel3(pixelbyte)=Panel3(pixelbyte) or 2^pixelpos
                elseif (b3-48)<48  then
                    'PANEL 4
                    pixel= (b1-16)*48 +b3-48
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel4(pixelbyte)=Panel4(pixelbyte) or 2^pixelpos
                else
        '            Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
         '           Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
                endif

            else
          '      Print "ROW EXCEPTION - DATA IS > 2 FRAME TALL"
           '     Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"

            endif
        b1= ((a2 shl 4) and &b11110000) + ((a3 shr 2) and &b00001111)
        b2= ((a3 shl 6) and &b11000000) + (a4 and &b00111111)
        sflag=2

    elseif sflag=2 then
        b3= ((a1 shl 2) and &b11111100) + ((a2 shr 4) and &b0000011)
        b4= ((a2 shl 4) and &b11110000) + ((a3 shr 2) and &b00001111)
    '          Print b1;b2;b3;b4;"*";

            if b1<16 then
                if b3<48  then
                    'PANEL 1
                    pixel= b1*48 +b3
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel1(pixelbyte)=panel1(pixelbyte) or 2^pixelpos
                elseif (b3-48)<48  then
                    'PANEL 2
                    pixel= b1*48 +b3-48
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel2(pixelbyte)=Panel2(pixelbyte) or 2^pixelpos
                else
        '            Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
         '           Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"

                endif
            elseif b1<32 then
                if b3<48  then
                    'PANEL 3
                    pixel= (b1-16)*48 +b3
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel3(pixelbyte)=Panel3(pixelbyte) or 2^pixelpos
                elseif (b3-48)<48  then
                    'PANEL 4
                    pixel= (b1-16)*48 +b3-48
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel4(pixelbyte)=Panel4(pixelbyte) or 2^pixelpos
                else
        '            Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
        '            Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
                endif
            else
         '      Print "ROW EXCEPTION - DATA IS > 2 FRAME TALL"
         '      Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"

            endif
        b1= ((a3 shl 6) and &b11000000) + (a4 and &b00111111)
        sflag=3

    elseif sflag=3 then
        b2= ((a1 shl 2) and &b11111100) + ((a2 shr 4) and &b0000011)
        b3= ((a2 shl 4) and &b11110000) + ((a3 shr 2) and &b00001111)
        b4= ((a3 shl 6) and &b11000000) + (a4 and &b00111111)
 '      Print b1;b2;b3;b4;"*";

            if b1<16 then
                if b3<48  then
                    'PANEL 1
                    pixel= b1*48 +b3
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel1(pixelbyte)=panel1(pixelbyte) or 2^pixelpos
                elseif (b3-48)<48  then
                    'PANEL 2
                    pixel= b1*48 +b3-48
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel2(pixelbyte)=Panel2(pixelbyte) or 2^pixelpos
                else
        '            Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
         '           Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"

                endif
            elseif b1<32 then
                if b3<48  then
                    'PANEL 3
                    pixel= (b1-16)*48 +b3
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel3(pixelbyte)=Panel3(pixelbyte) or 2^pixelpos
                elseif (b3-48)<48  then
                    'PANEL 4
                    pixel= (b1-16)*48 +b3-48
                    pixelbyte=int(pixel/8)
                    pixelpos=pixel-pixelbyte*8
                    panel4(pixelbyte)=Panel4(pixelbyte) or 2^pixelpos
                else
        '            Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
         '           Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
                endif
            else
          '      Print "ROW EXCEPTION - DATA IS > 2 FRAME TALL"
           '     Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"

            endif
        sflag=0
    endif


    z=z+4
    testlen=len(rd)
    if z>testlen then
        'Print
        'Print
        'Print "BREAK at:";
        'Print b64quad
        'sleep
        Input #infile,rd
        z=1
    endif
goto Panel_loop


'###########################################################################
' Now we have a frame Process the command that handles it
'###########################################################################
Framedone:

if cmd="#RL" then
           ' To scroll text Right to Left just take the edge of each of the frames and shift it

           p1byte1=0  '2 bytes that contain edge of frame to be rotated in
           p1byte2=0
           p2byte1=0
           p2byte2=0
           p3byte1=0
           p3byte2=0
           p4byte1=0
           p4byte2=0

           Z=128
           for rowCount=1 to 8
               yr=(rowCount*6)-1    'byte position end of row(s)1 to 8

               if bit(panel1(yr),7)=-1 then  ' right edge of frame rows 0 to 7
                   p1byte1=p1byte1+z
               end if

               if bit(panel2(yr),7)=-1 then  ' right edge of frame rows 0 to 7
                   p2byte1=p2byte1+z
               end if

               if bit(panel3(yr),7)=-1 then  ' right edge of frame rows 0 to 7
                   p3byte1=p3byte1+z
               end if

               if bit(panel4(yr),7)=-1 then  ' right edge of frame rows 0 to 7
                   p4byte1=p4byte1+z
               end if

               yr=((rowCount+8)*6)-1
               'byte position end of rows 8-15

               if bit(panel1(yr),7)=-1 then
                   p1byte2=p1byte2+z
               end if

               if bit(panel2(yr),7)=-1 then
                   p2byte2=p2byte2+z
               end if

               if bit(panel3(yr),7)=-1 then
                   p3byte2=p3byte2+z
               end if

               if bit(panel4(yr),7)=-1 then
                   p4byte2=p4byte2+z
               end if

               z=z/2
           next rowCount

         'Print THE 2 EDGE BYTES
           content=""
           if 1 then
               content=content+ chr$(126)+chr$(137)  'PANEL 2 RIGHT EDGE
               ' Escape encoding
               content=content+ escapeEncode(p1byte1)

               ' Escape encoding
               content=content+ escapeEncode(p1byte2)
           end if
           if 1 then
               content=content+ chr$(126)+chr$(145)  'PANEL 2 RIGHT EDGE

               ' Escape encoding
               content=content+ escapeEncode(p2byte1)

               ' Escape encoding
               content=content+ escapeEncode(p2byte2)
           end if
           if 1 then
               content=content+ chr$(126)+chr$(161)  'PANEL 2 RIGHT EDGE

               ' Escape encoding
               content=content+ escapeEncode(p3byte1)

               ' Escape encoding
               content=content+ escapeEncode(p3byte2)
           end if
           if 1 then
               content=content+ chr$(126)+chr$(193)  'PANEL 2 RIGHT EDGE

               ' Escape encoding
               content=content+ escapeEncode(p4byte1)

               ' Escape encoding
               content=content+ escapeEncode(p4byte2)
           end if
           put #comport,,content
           sendUdp

           tdelayms=(timer-lastmark)*1000
           delay=hedgedelay-tdelayms
           if delay>0 then
                 sleep delay
           else
                 Print"!";
           endif
           lastmark=timer

elseif cmd="#RR" then
' To scroll text Left to Right

           p1byte1=0  '2 bytes that contain edge of frame to be rotated in
           p1byte2=0
           p2byte1=0
           p2byte2=0
           p3byte1=0
           p3byte2=0
           p4byte1=0
           p4byte2=0
            Z=128
            for rowCount=1 to 8

                yl=(rowCount-1)*6    'byte position start of rows

                if bit(panel1(yl),7)=-1 then  ' left edge of frame rows 0 to 7
                      p1byte1=p1byte1+z
                end if

                if bit(panel2(yl),7)=-1 then  ' left edge of frame rows 0 to 7
                      p2byte1=p2byte1+z
                end if

                if bit(panel3(yl),7)=-1 then  ' left edge of frame rows 0 to 7
                      p3byte1=p3byte1+z
                end if

                if bit(panel4(yl),7)=-1 then  ' left edge of frame rows 0 to 7
                      p4byte1=p4byte1+z
                end if

                'byte position end of rows 8-15
                yl=(rowCount-1+8)*6
                'byte position start of rows 8-15
                if bit(panel1(yl),7)=-1 then  ' left edge of frame rows 8-15
                      p1byte2=p1byte2+z
                end if
                if bit(panel2(yl),7)=-1 then  ' left edge of frame rows8-15
                      p2byte2=p2byte2+z
                end if

                if bit(panel3(yl),7)=-1 then  ' left edge of frame rows 8-15
                      p3byte2=p3byte2+z
                end if

                if bit(panel4(yl),7)=-1 then  ' left edge of frame rows 8-15
                      p4byte2=p4byte2+z
                end if

                z=z/2
            next rowCount
            'Print THE 2 EDGE BYTES
            content=""
           if 1 then
                          content=content+ chr$(126)+chr$(138)  'PANEL 2 RIGHT EDGE

                          ' Escape encoding
                          content=content+ escapeEncode(p1byte1)
                          content=content+ escapeEncode(p1byte2)
           end if
           if 1 then
                          content=content+ chr$(126)+chr$(146)  'PANEL 2 RIGHT EDGE

                          content=content+ escapeEncode(p2byte1)
                          content=content+ escapeEncode(p2byte2)
           end if
           if 1 then
                          content=content+ chr$(126)+chr$(162)  'PANEL 2 RIGHT EDGE

                          content=content+ escapeEncode(p3byte1)
                          content=content+ escapeEncode(p3byte2)
           end if
           if 1 then
                          content=content+ chr$(126)+chr$(194)  'PANEL 2 RIGHT EDGE
                          content=content+ escapeEncode(p4byte1)
                          content=content+ escapeEncode(p4byte2)
           end if

            put #comport,,content
            sendUdp

            tdelayms=(timer-lastmark)*1000
            delay=hedgedelay-tdelayms
            if delay>0 then
                   sleep delay
            else
                   Print"!";
            endif
            lastmark=timer

elseif cmd = "#RD" then
            content=""
            if 1 then
                    content=content +chr$(126)+chr$(140)
                    for rowCount= 5 to 0 step -1
                          byte1=panel1(rowCount)
                          content=content+ escapeEncode(byte1)
                    next rowCount
            endif
            if 1 then
                    content=content+chr$(126)+chr$(148)
                    for rowCount= 5 to 0 step -1
                          byte1=panel2(rowCount)
                          content=content+ escapeEncode(byte1)
                    next rowCount
            endif
            if 1 then
                    content=content+chr$(126)+chr$(164)
                    for rowCount= 5 to 0 step -1
                          byte1=panel3(rowCount)
                          content=content+ escapeEncode(byte1)
                    next rowCount
            endif
            if 1 then
                    content=content+chr$(126)+chr$(196)
                    for rowCount= 5 to 0 step -1
                          byte1=panel4(rowCount)
                          content=content+ escapeEncode(byte1)
                    next rowCount
            endif

            put #comport,,content
            sendUdp

            tdelayms=(timer-lastmark)*1000
            delay=vedgedelay-tdelayms
            if delay>0 then
                   sleep delay
            endif
            lastmark=timer


elseif cmd = "#RU" then
            content=""
            if 1 then
                    content=content+chr$(126)+chr$(139)
                    for byteCount= 95 to 90 step -1
                          byte1=panel1(byteCount)
                          content=content+ escapeEncode(byte1)
                    next byteCount
            endif
            if 1 then
                    content=content+chr$(126)+chr$(147)
                    for byteCount= 95 to 90 step -1
                          byte1=panel2(byteCount)
                          content=content+ escapeEncode(byte1)
                    next byteCount
            endif
            if 1 then
                    content=content+chr$(126)+chr$(163)
                    for byteCount= 95 to 90 step -1
                          byte1=panel3(byteCount)
                          content=content+ escapeEncode(byte1)
                    next byteCount
            endif
            if 1 then
                    content=content+chr$(126)+chr$(195)
                    for byteCount= 95 to 90 step -1
                          byte1=panel4(byteCount)
                          content=content+ escapeEncode(byte1)
                    next byteCount
            endif

            put #comport,,content
            sendUdp

             tdelayms=(timer-lastmark)*1000
             delay=vedgedelay-tdelayms
             if delay>0 then
                    sleep delay
             else
                    Print"!";
             endif
             lastmark=timer

elseif cmd = "#FRM" then
content=""
'Panel 3 (165)
                    if panelCount >2 then
                        content=content +chr$(126)+chr$(165)
                        for byteCount= 95 to 0 step -1
                              byte1=panel3(byteCount)
                              content=content+ escapeEncode(byte1)
                        next byteCount
                    else        ' just in case we're scrolling....
                       content=content+chr$(126)+Chr$(166) 'clear panel 3
                    endif


'Panel 1 141
                    if 1 then  'always do panel 1

                        content=content +chr$(126)+chr$(141)
                        for byteCount= 95 to 0 step -1
                              byte1=panel1(byteCount)
                              content=content+ escapeEncode(byte1)
                        next byteCount
                    endif


'Panel 4 197
                    if panelCount>3 then

                        content=content +chr$(126)+chr$(197)
                        for byteCount= 95 to 0 step -1
                              byte1=panel4(byteCount)
                              content=content+ escapeEncode(byte1)
                         next byteCount

                    endif

'Panel 2
                    if panelCount>1 then
                        'if rows = 2 = panel 2
                        'if rowa =1 NA
                        content=content +chr$(126)+chr$(149)
                        for byteCount= 95 to 0 step -1
                              byte1=panel2(byteCount)
                              content=content+ escapeEncode(byte1)
                         next byteCount
                    endif

                    put #comport,,content
                    sendUdp

            tdelayms=(timer-lastmark)*1000
            delay=framedelay-tdelayms
            if delay>0 then
                for x=1 to delay/2
                    sleep 2
                next x
            else
                    'Print"!";
            endif
            lastmark=timer
elseif left(cmd,2) = "#W" then
    file_id=mid(cmd,3,2)
    file_id=file_id+".dat"
    if file_is_open=0 then
        kill file_id
        outfile=freefile
        open file_id for append as #outfile
        if err then
            Print "Output file cannot be opened"
            sleep
            end
        else
            Print "Output file:";file_id;" opened OK"
            file_is_open=1
        endif
    endif

    content=""
    'Panel 3 (165)
    content=content +chr$(126)+chr$(165)
    for x= 95 to 0 step -1
        byte1=panel3(x)
        content=content+ chr$(byte1)
    next x

    'Panel 1 141
    content=content +chr$(126)+chr$(141)
    for x= 95 to 0 step -1
        byte1=panel1(x)
        content=content+ chr$(byte1)
    next x

    'Panel 4 197
    content=content +chr$(126)+chr$(197)
    for x= 95 to 0 step -1
        byte1=panel4(x)
        content=content+ chr$(byte1)
    next x

    'Panel 2
    'if rows = 2 = panel 2
    'if rowa =1 NA
    content=content +chr$(126)+chr$(149)
    for x= 95 to 0 step -1
        byte1=panel2(x)
        content=content+ chr$(byte1)
    next x
    content=content+string(120,0)

    put #outfile,,content

else

    Print "COMMAND NOT RECOGNISED: '";cmd;"'"
    sleep
    goto byebye
endif

Endif      ' processing the frame

'FRAME DIAGNOSTIC
If diagFlag= 1 then
    Print
    Print  "------------------------------------------------------------------------------------------------"
    for rowCount=0 to 15
        for columnCount= 0 to 5
            for bitCount= 0 to 7
                if bit(panel1(rowCount*6+columnCount),bitCount)=-1 then
                    Print "*";
                else
                    Print " ";
                endif
            next bitCount
        next columnCount

        for columnCount= 0 to 5
            for bitCount= 0 to 7
                if bit(panel2(rowCount*6+columnCount),bitCount)=-1 then
                    Print "0";
                else
                    Print " ";
                endif
            next bitCount
        next columnCount
        Print " |"
    next rowCount

    for rowCount=0 to 15
        for columnCount= 0 to 5
            for bitCount= 0 to 7
                if bit(panel3(rowCount*6+columnCount),bitCount)=-1 then
                    Print "X";
                else
                    Print " ";
                endif
            next bitCount
        next columnCount

        for columnCount= 0 to 5
            for bitCount= 0 to 7
                if bit(panel4(rowCount*6+columnCount),bitCount)=-1 then
                    Print "#";
                else
                    Print " ";
                endif
            next bitCount
        next columnCount
        Print " |"
    next rowCount

    Print "------------------------------------------------------------------------------------------------"
    sleep
endif

No_Display:

goto getdata

'##########################################################################
' EXIT -OUR WORK HERE IS DONE
'##########################################################################

ByeBye:
    Close #infile
    if file_is_open=1 then
        Close #outfile
    endif
Goto next_parameter

no_params:
    Close #comport

    if (sock) then
       closesocket(sock)
    end if
    END

'#########################################################################
' SETTING UP TRIKSC.DAT
'#########################################################################

setup:
cls
Print "##################################"
Print "TRIKS-C Setup"
Print "TESTING FOR COMPORTS (COM1:-COM40)"
Print "##################################"
testPorts
Print

getPort:
input "SETUP: Enter the COM port you wish to use: eg 'COM1:' : ",theport
if instr(lcase(theport),"com")=0 or instr(theport,":")=0 then
    Print " THE FORMAT MUST BE 'COMx:' where x is your port number"
    goto getPort
endif

comport=freefile
theport1=theport+" 57600,n,8,1,cs0,ds0,cd0,rs"
open com theport1 as #comport
if err then
    Print "Port ";theport;" cannot be opened"
    sleep
    end
else
    close #comport
    kill "triksc.dat"
    parameters=freefile
    open "triksc.dat" for output as #parameters
    if err then
        Print "TRIKSC.DAT cannot be opened"
        sleep
        end
    endif
    input "How many panels are on display (1-4)",panelCount
    input "Now enter the frame delay (ms) (normal= 50)",framedelay
    input "Now enter the horizontal edge scrolldelay (ms) (normal=50)",hedgedelay
    input "Now enter the vertical edge scroll delay (ms) (normal= 100)",vedgedelay
    input "Now enter the rotational delay (normal = 0)",rotdelay

    Print #parameters,theport
    strDelay=str(panelCount)
    Print #parameters,strDelay
    strDelay=str(framedelay)
    Print #parameters,strDelay
    strDelay=str(hedgedelay)
    Print #parameters,strDelay
    strDelay=str(vedgedelay)
    Print #parameters,strDelay
    strDelay=str(rotdelay)
    Print #parameters,strDelay
    close
endif
goto start

' ----------------------------------------------
' Subroutine to handle image rotation
' ----------------------------------------------
Sub handleRotation(direction as integer)
   DIM as integer DEGREE, rotpos
   DIM as ubyte panel1ROT(95) ' Frame buffers
   DIM as ubyte panel2ROT(95)
   DIM as ubyte panel3ROT(95)
   DIM as ubyte panel4ROT(95)
   DIM as integer byteIdx,rowIdx,bitIdx,xa
   DIM as double RAD, radpos
   DIM as double r,y1,x1
   DIM as integer pixel,pixel1,pixelbyte,pixelpos ',rows,cols
   DIM as string content
   DIM as ubyte byte1

   rotpos = 0
   FOR DEGREE=rotpos TO rotpos+Rto*direction STEP Rinc*direction
       erase panel1rot
       erase panel2rot
       erase panel3rot
       erase panel4rot

       FOR rowIdx=0 TO 15
           FOR byteIdx=0 TO 5
               For bitIdx=0 to 7
                   Xa=byteIdx*8+bitIdx

                   'Look at Panel 1 bits
                   radpos=atan2((rowIdx-RYC),(RXC-xa))
                   RAD=degree*pi/180
                   if bit(panel1(rowIdx*6+byteIdx),bitIdx)=-1 then
                       R=sqr((Xa-RXC)^2+(rowIdx-RYC)^2)
                       Y1=int(RYC-R*sin(RAD-radpos)+0.5)
                       X1=int(RXC-R*cos(RAD-radpos)+0.5)
                       if not (y1<0 or y1>15 or x1<0 or x1>47) then
                          PIXEL1=INT(X1+0.5)+INT(Y1+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel1ROT(pixelbyte)=panel1ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<0 or y1>15 or x1<48 or x1>95) then
                          PIXEL1=INT(X1-48+0.5)+INT(Y1+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel2ROT(pixelbyte)=Panel2ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<16 or y1>31 or x1<0 or x1>47) then
                          PIXEL1=INT(X1+0.5)+INT(Y1-16+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel3ROT(pixelbyte)=Panel3ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<16 or y1>31 or x1<48 or x1>95) then
                          PIXEL1=INT(X1-48+0.5)+INT(Y1-16+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel4ROT(pixelbyte)=Panel2ROT(pixelbyte)+2^pixelpos
                       endif
                   end if


                   'Look at Panel 2 bits (need to add 48 to xa)
                   radpos=atan2((rowIdx-RYC),(RXC-(xa+48)))
                   RAD=degree*pi/180
                   if bit(panel2(rowIdx*6+byteIdx),bitIdx)=-1 then
                       R=sqr((Xa+48-RXC)^2+(rowIdx-RYC)^2)
                       Y1=int(RYC-R*sin(RAD-radpos)+0.5)
                       X1=int(RXC-R*cos(RAD-radpos)+0.5)
                       if not (y1<0 or y1>15 or x1<0 or x1>47) then
                          PIXEL1=INT(X1+0.5)+INT(Y1+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel1ROT(pixelbyte)=panel1ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<0 or y1>15 or x1<48 or x1>95) then
                          PIXEL1=INT(X1-48+0.5)+INT(Y1+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel2ROT(pixelbyte)=Panel2ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<16 or y1>31 or x1<0 or x1>47) then
                          PIXEL1=INT(X1+0.5)+INT(Y1-16+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel3ROT(pixelbyte)=Panel3ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<16 or y1>31 or x1<48 or x1>95) then
                          PIXEL1=INT(X1-48+0.5)+INT(Y1-16+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel4ROT(pixelbyte)=Panel2ROT(pixelbyte)+2^pixelpos
                       endif
                   end if

                   'Look at Panel 3 bits (need to add 16 to y)
                   radpos=atan2((rowIdx+16-RYC),(RXC-xa))
                   RAD=degree*pi/180
                   if bit(panel3(rowIdx*6+byteIdx),bitIdx)=-1 then
                       R=sqr((Xa-RXC)^2+(rowIdx+16-RYC)^2)
                       Y1=int(RYC-R*sin(RAD-radpos)+0.5)
                       X1=int(RXC-R*cos(RAD-radpos)+0.5)
                       if not (y1<0 or y1>15 or x1<0 or x1>47) then
                          PIXEL1=INT(X1+0.5)+INT(Y1+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel1ROT(pixelbyte)=panel1ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<0 or y1>15 or x1<48 or x1>95) then
                          PIXEL1=INT(X1-48+0.5)+INT(Y1+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel2ROT(pixelbyte)=Panel2ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<16 or y1>31 or x1<0 or x1>47) then
                          PIXEL1=INT(X1+0.5)+INT(Y1-16+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel3ROT(pixelbyte)=Panel3ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<16 or y1>31 or x1<48 or x1>95) then
                          PIXEL1=INT(X1-48+0.5)+INT(Y1-16+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel4ROT(pixelbyte)=Panel2ROT(pixelbyte)+2^pixelpos
                       endif
                   end if

                   'Look at Panel 4 bits (need to add 16 to y and 48 to xa)
                   radpos=atan2((rowIdx+16-RYC),(RXC-(xa+48)))
                   RAD=degree*pi/180
                   if bit(panel4(rowIdx*6+byteIdx),bitIdx)=-1 then
                       R=sqr((Xa+48-RXC)^2+(rowIdx+16-RYC)^2)
                       Y1=int(RYC-R*sin(RAD-radpos)+0.5)
                       X1=int(RXC-R*cos(RAD-radpos)+0.5)
                       if not (y1<0 or y1>15 or x1<0 or x1>47) then
                          PIXEL1=INT(X1+0.5)+INT(Y1+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel1ROT(pixelbyte)=panel1ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<0 or y1>15 or x1<48 or x1>95) then
                          PIXEL1=INT(X1-48+0.5)+INT(Y1+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel2ROT(pixelbyte)=Panel2ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<16 or y1>31 or x1<0 or x1>47) then
                          PIXEL1=INT(X1+0.5)+INT(Y1-16+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel3ROT(pixelbyte)=Panel3ROT(pixelbyte)+2^pixelpos
                       elseif not(y1<16 or y1>31 or x1<48 or x1>95) then
                          PIXEL1=INT(X1-48+0.5)+INT(Y1-16+0.5)*48
                          pixelbyte=int(pixel1/8)
                          pixelpos=pixel1-int(pixel1/8)*8
                          panel4ROT(pixelbyte)=Panel2ROT(pixelbyte)+2^pixelpos
                       endif
                   end if

               NEXT bitIdx
           Next byteIdx
       Next rowIdx

'FRAME DIAGNOSTIC
If diagFlag= 1 then
       Print
       Print  "------------------------------------------------------------------------------------------------"
       for rowIdx=0 to 15
           for byteIdx= 0 to 5
               for bitIdx= 0 to 7
                   if bit(panel1rot(rowIdx*6+byteIdx),bitIdx)=-1 then
                       Print "*";
                   else
                       Print " ";
                   endif
                next bitIdx
           next byteIdx

           for byteIdx= 0 to 5
               for bitIdx= 0 to 7
                   if bit(panel2rot(rowIdx*6+byteIdx),bitIdx)=-1 then
                       Print "0";
                   else
                       Print " ";
                   endif
               next bitIdx
           next byteIdx

           Print " |"
       next rowIdx

       for rowIdx=0 to 15
           for byteIdx= 0 to 5
               for bitIdx= 0 to 7
                   if bit(panel3rot(rowIdx*6+byteIdx),bitIdx)=-1 then
                       Print "X";
                   else
                       Print " ";
                    endif
                next bitIdx
            next byteIdx

            for byteIdx= 0 to 5
                for bitIdx= 0 to 7
                    if bit(panel4rot(rowIdx*6+byteIdx),bitIdx)=-1 then
                        Print "#";
                    else
                        Print " ";
                    endif
                next bitIdx
            next byteIdx
            Print " |"
        next rowIdx

        Print "------------------------------------------------------------------------------------------------"
        sleep
endif

       content=""
       content= content+chr$(126)+chr$(141)
       'for byteIdx= 95 to 0 step -1
       for byteIdx= 0 to 95 step 1
           byte1=panel1rot(byteIdx)
           content=content+ escapeEncode(byte1)
        next byteIdx

        content=content+chr$(126)+chr$(149)
        'for byteIdx= 95 to 0 step -1
        for byteIdx= 0 to 95 step 1
            byte1=panel2rot(byteIdx)
            content=content+ escapeEncode(byte1)
        next byteIdx

        content=content+chr$(126)+chr$(165)
        'for byteIdx= 95 to 0 step -1
        for byteIdx= 0 to 95 step 1
            byte1=panel3rot(byteIdx)
            content=content+ escapeEncode(byte1)
        next byteIdx

        content=content+chr$(126)+chr$(197)
        'for byteIdx= 95 to 0 step -1
        for byteIdx= 0 to 95 step 1
            byte1=panel4rot(byteIdx)
            content=content+ escapeEncode(byte1)
        next byteIdx
        put #comport,,content

        tdelayms=(timer-lastmark)*1000
        delay=rotdelay-tdelayms
        if delay>0 then
            sleep delay
        endif
        lastmark=timer
    next DEGREE
    rotpos= (rotpos+rto*direction) mod 360
end Sub

Sub printCmdHelp
         CLS
         Print
         Print
         Print"**************************************************************"
         Print"*   LEDTRIKS-C COMMAND LINE INTERFACE PROGRAM                *"
         Print"*   BETA RELEASE 05/9/10  Tim Wells      V0.11               *"
         Print"**************************************************************"
         Print
         Print" This version can drive up to 2x2 TriksC/Ledtriks panels"
         Print
         Print "USAGE: LTC [filename] command [filename] [command] "
         Print "Commands:"
         Print "            #CLS     = clear screen"
         Print "            #BLK     = blank screen"
         Print "            #SHW     = Show screen"
         Print "            #RL      = Roll to Left (EDGE TRANSMISSION)"
         Print "            #RR      = Roll to Right"
         Print "            #RD      = Roll Down"
         Print "            #RU      = Roll UP"
         Print "            #FRM     = Whole Frame"
         Print
         Print "            #RPTxxxx = Repeat all xx times"
         Print "            #DLYnnnn = Wait nnnn ms"
         Print "            #LOOP    = Repeat until keypress"

         Print "            #FDLnnnn = Delay between Frames nnnn (ms)"
         Print "            #HDLnnnn = Delay for Horizontal scroll (ms)"
         Print "            #VDLnnnn = Delay for Vertical scroll (ms)"
         Print "            #RDLnnnn = Delay for Rotate (ms)"
         Print "            #NPx     = Number of active panels (1-4)"
         Print
         Print "            #TIME    = Display time to execute this file"
         Print
         Print "            #ROTR    = Rotate Clockwise (right)"
         Print "            #ROTL    = Rotate AntiClockwise (left)"
         Print "            #RINCnnn = Rotation Steps (nnn degrees)"
         Print "            #RTOnnn  = Rotation Angle (nnn degrees)"
         Print "            #RXCnnnn = Rotate Center X co-ord"
         Print "            #RYCnnnn = Rotate Center Y co-ord"
         Print ""
'         Print "            #Wxx     = Write Triks SD data (file xx.dat)"
 '        Print "            #TSDxx   = Order TRIKS SD to transmit file xx.*"
         Print
         Print "            #STA     = Send TRIKS-C to stand alone mode"
         Print "                       (until next command received)"
         Print
         Print "            #CALxxx  = Coundown timer (2panels) xxx secs"
         Print "            #CLKxxx  = Clock (1 Panel) for xxx secs"
         Print
         Print " (OR LTC [filename] with no params to run a script file "
         Print "  with 1 command per line)"
         Print
         Print " (TRIKSC.DAT stores comport in use- delete it to choose comport)"
         Print
         Sleep
end Sub

Sub testPorts
    DIM idx as integer
    DIM strPortParam as string
    DIM as integer port

    for idx=1 to 40
        port=freefile
        strPortParam="COM"+str(idx)+": 57600,n,8,1,cs0,ds0,cd0,rs"
        open com strPortParam as #port
        if ERR then
            'nothing
        else
            Print "COM";str(idx);": EXISTS"
            close #port
        endif
    next idx
end Sub


dataInit:
'	DIGIT0
    Data	0,1,1,1,0
    Data	1,0,0,0,1
    Data	1,0,0,0,1
    Data	1,0,0,0,1
    Data	1,0,0,0,1
    Data	1,0,0,0,1
    Data	0,1,1,1,0
'	DIGIT1
    Data	0,0,0,1,0
    Data	0,0,1,1,0
    Data	0,1,0,1,0
    Data	0,0,0,1,0
    Data	0,0,0,1,0
    Data	0,0,0,1,0
    Data	0,0,0,1,0
'	DIGIT2
    Data	0,1,1,1,0
    Data	1,0,0,0,1
    Data	0,0,0,0,1
    Data	0,0,0,1,0
    Data	0,0,1,0,0
    Data	0,1,0,0,0
    Data	1,1,1,1,1
'	DIGIT3
    Data	0,1,1,1,0
    Data	1,0,0,0,1
    Data	0,0,0,0,1
    Data	0,0,1,1,0
    Data	0,0,0,0,1
    Data	1,0,0,0,1
    Data	0,1,1,1,0
'	DIGIT4
    Data	0,0,0,1,0
    Data	0,0,1,1,0
    Data	0,1,0,1,0
    Data	1,0,0,1,0
    Data	1,1,1,1,1
    Data	0,0,0,1,0
    Data	0,0,0,1,0
'	DIGIT5
    Data	1,1,1,1,1
    Data	1,0,0,0,0
    Data	1,1,1,1,0
    Data	0,0,0,0,1
    Data	0,0,0,0,1
    Data	1,0,0,0,1
    Data	0,1,1,1,0
'	DIGIT6
    Data	0,1,1,1,0
    Data	1,0,0,0,0
    Data	1,0,0,0,0
    Data	1,1,1,1,0
    Data	1,0,0,0,1
    Data	1,0,0,0,1
    Data	0,1,1,1,0
'	DIGIT7
    Data	1,1,1,1,1
    Data	0,0,0,0,1
    Data	0,0,0,1,1
    Data	0,0,1,1,0
    Data	0,1,1,0,0
    Data	1,1,0,0,0
    Data	1,0,0,0,0
'	DIGIT8
    Data	0,1,1,1,0
    Data	1,0,0,0,1
    Data	1,0,0,0,1
    Data	0,1,1,1,0
    Data	1,0,0,0,1
    Data	1,0,0,0,1
    Data	0,1,1,1,0
'	DIGIT9
    Data	0,1,1,1,0
    Data	1,0,0,0,1
    Data	1,0,0,0,1
    Data	0,1,1,1,1
    Data	0,0,0,0,1
    Data	0,0,0,0,1
    Data	0,1,1,1,0

'Load the Digitsfor countdown
    For x =0 to 9
        for y= 0 to 34
            read digits(x,y)
        next y
    Next x

' Clock Digit 0
    Data	0,1,1,1,1,0,0
    Data	0,1,1,1,1,0,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,1,1,1,1,0
    Data	0,1,1,1,1,0,0
' Clock Digit 1
    Data	0,0,0,1,1,0,0
    Data	0,0,0,1,1,0,0
    Data	0,0,1,1,1,0,0
    Data	0,1,1,1,1,0,0
    Data	0,1,0,1,1,0,0
    Data	0,0,0,1,1,0,0
    Data	0,0,0,1,1,0,0
    Data	0,0,0,1,1,0,0
    Data	0,0,0,1,1,0,0
    Data	0,0,0,1,1,0,0
    Data	0,0,0,1,1,0,0

' Clock Digit 2
    Data	0,0,1,1,1,0,0
    Data	0,1,1,1,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	0,0,0,0,1,1,0
    Data	0,0,0,1,1,0,0
    Data	0,0,1,1,1,0,0
    Data	0,1,1,1,0,0,0
    Data	0,1,1,0,0,0,0
    Data	1,1,1,1,1,1,0
    Data	1,1,1,1,1,1,0

' Clock Digit 3
    Data	0,1,1,1,1,0,0
    Data	1,1,1,1,1,1,0
    Data	1,1,0,0,1,1,0
    Data	0,0,0,0,1,1,0
    Data	0,0,1,1,1,0,0
    Data	0,0,1,1,1,0,0
    Data	0,0,0,0,1,1,0
    Data	0,0,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,1,1,1,1,0
    Data	0,1,1,1,1,0,0

' Clock Digit 4
    Data	0,0,0,0,1,1,0
    Data	0,0,0,1,1,1,0
    Data	0,0,0,1,1,1,0
    Data	0,0,1,1,1,1,0
    Data	0,0,1,0,1,1,0
    Data	0,1,1,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,1,1,1,1,0
    Data	1,1,1,1,1,1,0
    Data	0,0,0,0,1,1,0
    Data	0,0,0,0,1,1,0

' Clock Digit 5
    Data	0,1,1,1,1,1,0
    Data	0,1,1,1,1,1,0
    Data	0,1,1,0,0,0,0
    Data	1,1,0,1,1,0,0
    Data	1,1,1,1,1,1,0
    Data	1,1,0,0,1,1,0
    Data	0,0,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	0,1,1,1,1,0,0
    Data	0,1,1,1,1,0,0

' Clock Digit 6
    Data	0,1,1,1,0,0,0
    Data	0,1,1,1,1,0,0
    Data	1,1,0,1,1,0,0
    Data	1,1,0,0,0,0,0
    Data	1,1,1,1,0,0,0
    Data	1,1,1,1,1,0,0
    Data	1,1,0,1,1,0,0
    Data	1,1,0,1,1,0,0
    Data	1,1,0,1,1,0,0
    Data	0,1,1,1,1,0,0
    Data	0,1,1,1,0,0,0

' Clock Digit 7
    Data	0,1,1,1,1,1,0
    Data	0,1,1,1,1,1,0
    Data	0,0,0,0,1,1,0
    Data	0,0,0,0,1,0,0
    Data	0,0,0,1,1,0,0
    Data	0,0,0,1,1,0,0
    Data	0,0,0,1,0,0,0
    Data	0,0,1,1,0,0,0
    Data	0,0,1,1,0,0,0
    Data	0,0,1,1,0,0,0
    Data	0,0,1,1,0,0,0

' Clock Digit 8
    Data	0,1,1,1,1,0,0
    Data	1,1,1,1,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	0,1,1,1,1,0,0
    Data	0,1,1,1,1,0,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,1,1,1,1,0
    Data	0,1,1,1,1,0,0

' Clock Digit 9
    Data	0,1,1,1,0,0,0
    Data	1,1,1,1,1,0,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,1,1,1,1,0
    Data	0,1,1,1,1,1,0
    Data	0,0,0,0,1,1,0
    Data	1,1,0,0,1,1,0
    Data	1,1,1,1,1,0,0
    Data	0,1,1,1,0,0,0

' Clock Digit 10 (blank)
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
    Data	0,0,0,0,0,0,0
'Load the Digits for clock
    For x =0 to 10
        for y= 0 to 76
            read clockDigits(x,y)
        next y
    Next x

'DEFAULT FRAME01:
    Data 14, 0, 64, 16, 0, 0
    Data 17, 0, 64, 16, 0, 0
    Data 129, 73, 231, 156, 73, 58
    Data 65, 74, 73, 82, 170, 74
    Data 65, 74, 73, 82, 170, 74
    Data 81, 74, 73, 82, 170, 74
    Data 142, 113, 201, 156, 17, 73
    Data 0, 0, 0, 0, 0, 0
    Data 0, 0, 0, 0, 0, 0
    Data 0, 0, 0, 0, 0, 0
    Data 0, 0, 0, 0, 0, 0
    Data 0, 0, 63, 0, 0, 14
    Data 0, 0, 73, 0, 0, 1
    Data 0, 0, 73, 0, 0, 6
    Data 0, 0, 73, 0, 0, 8
    Data 0, 0, 73, 0, 0, 7
'DEFAULT FRAME02:
    Data 112, 2, 2, 2, 0, 0
    Data 136, 2, 0, 2, 0, 0
    Data 8, 222, 114, 247, 227, 28
    Data 8, 82, 10, 146, 132, 2
    Data 8, 82, 50, 146, 228, 12
    Data 136, 82, 66, 146, 148, 16
    Data 112, 82, 58, 150, 244, 14
    Data 0, 0, 0, 0, 0, 0
    Data 0, 0, 0, 0, 0, 0
    Data 0, 0, 0, 2, 0, 8
    Data 0, 0, 0, 2, 0, 8
    Data 0, 0, 128, 3, 0, 56
    Data 0, 0, 64, 2, 0, 72
    Data 0, 0, 64, 2, 0, 72
    Data 0, 0, 64, 2, 0, 72
    Data 0, 0, 128, 3, 0, 72

    for byteCount=0 to 95
        read panel1(byteCount)
        for y= 0 to 7
            if bit(panel1(byteCount),y)=-1 then
                Panel_1(byteCount*8+y)=1
            endif
        next y
    next byteCount
    for byteCount=0 to 95
        read panel2(byteCount)
        for y= 0 to 7
            if bit(panel2(byteCount),y)=-1 then
                Panel_2(byteCount*8+y)=1
            endif
        next y
    next byteCount
goto start
