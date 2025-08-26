/* ****************************************************************************
 LTC - SERIAL DRIVER FOR LEDTRIKS        (c) Copyright Tim Wells

 VERSION 0.11    LTC_11.C         5/9/2010

 AI port to C, 2025/08/20 (Aurbo99)

 2025/07/24 (LabRat) code clean-up (and network streaming?)

 05/09/10 Moved single panel clock to panel 1

 Added PixC support

 07/8/10 Added auto christmas detection

         Added single panel clock

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  This program is provided free for you to use in any way that you wish,
  subject to the laws and regulations where you are using it.  Due diligence
  is strongly suggested before using this code.

  The Author makes no warranty of any kind, express or implied, with regard
  to this program or the documentation contained in this document.  The
  Author shall not be liable in any event for incidental or consequential
  damages in connection with, or arising out of, the furnishing, performance
  or use of these programs.
 It is released to assist in explaining the concept only and may not work
 in all configs.

  ############################################################################## */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <winsock2.h>
#include <time.h>
#include <unistd.h> // sleep()
#include <ctype.h> // toupper()
#include "LTC_UDP.h"

// Constants for Winsock initialization
#define WS_VERSION_REQUEST 0x0202 // Winsock 2.2

// -----------------
// var TIME related:
// -----------------
time_t currentTimeDate, dateDec25;
char strDaysRemaining[100], strTimeRemaining[100], strTime[100];
int d_h, d_t, d_u, H_t, H_u, M_t, M_u, S_t, S_u, start, currentYear;
int previousS_u=-1;

// Look until actual time > gTimeout
double gActualTime, gTimeout;

// --------------------
// FrameBuffer BitMap
// --------------------
unsigned char Panel_1[768], Panel_2[768];
int digits[10][35], clockDigits[11][77];

// --------------------
// FrameBuffers ByteMap
// --------------------
unsigned char panel1[96];
unsigned char panel2[96];
unsigned char panel3[96];
unsigned char panel4[96];

// ------------
// File access
// ------------
FILE *scriptfile, *infile, *outfile;
char inputfile[100], scriptfilename[100], file_id[100];

// ----------------
// Com Port access
// ----------------
FILE *comport;
char sComOpenCmd[100], theport[100], theport1[100], strComPort[100];

// ---------------------
// Command Line Parsing
// ---------------------
int argc, parameters;
char argv[100], cmd[100];

// ------------------
// Winsock Variables
// ------------------
WSADATA wsaData;
SOCKET sock;

// Panel Selection
int p_pan, p_pan1;
char p_select[100], p_select1[100];

// Repeat count
int repeatCount;

// Config parameters
int panelCount;
int framedelay, hedgedelay, vedgedelay;
int rotdelay;

// Other variables
int pixel, pixel1, pixelbyte, pixelpos, rows, cols;
int diagFlag=0;
int p1byte1, p1byte2, p2byte1, p2byte2, p3byte1, p3byte2, p4byte1, p4byte2;
int a1, a2, a3, a4, b1, b2, b3, b4, sflag, pflag, testlen;
int yl, yr, use_script;
int X, Y, Z;
int rowCount, columnCount, bitCount, byteCount;
int delay;
int nocommand;
int file_is_open=0;

unsigned char byte1;
int direction;
int Rto, Rinc, RXC, RYC;
char RD[100], B64quad[100], rextra[100], strDelay[100];
char content[100];
double starttime;
double lastmark, tdelayms;

const double PI = 3.1415926535897932;
const char* B64decode = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/**
 * @brief Calculates the number of days until the next Christmas.
 *
 * This function determines the number of days remaining until December 25th.
 * If the current date is on or after December 25th of the current year,
 * it calculates the days until Christmas of the following year.
 *
 * @return The number of days until the next Christmas.
 */

int daysUntilChristmas() {
    time_t rawtime;
    struct tm *info;
    struct tm christmas_date;
    double seconds_diff;
    int days_left;

    // Get current time
    time(&rawtime);
    info = localtime(&rawtime);

    // Initialize Christmas date structure
    christmas_date = *info; // Copy current date information
    christmas_date.tm_mon = 11; // December (0-indexed)
    christmas_date.tm_mday = 25; // 25th

    // If Christmas has already passed this year, set target to next year
    if (info->tm_mon > 11 || (info->tm_mon == 11 && info->tm_mday > 25)) {
        christmas_date.tm_year++;
    }

    // Normalize the tm structure to ensure correct mktime conversion
    christmas_date.tm_hour = 0;
    christmas_date.tm_min = 0;
    christmas_date.tm_sec = 0;
    christmas_date.tm_isdst = -1; // Let mktime determine DST

    // Convert tm structures to time_t (seconds since epoch)
    time_t current_time_t = mktime(info);
    time_t christmas_time_t = mktime(&christmas_date);

    // Calculate difference in seconds
    seconds_diff = difftime(christmas_time_t, current_time_t);

    // Convert seconds to days
    days_left = (int)(seconds_diff / (60 * 60 * 24));

    return days_left;
}


//##############################################################################
//SUBS
//##############################################################################
void handleRotation(int direction);
void printCmdHelp();
void testPorts();
void setup();
void no_params();
void ByeBye();

char* escapeEncode(int outchar) {
    static char encoded[3]; // Buffer to hold the encoded characters
    if (outchar > 124 && outchar < 128) {
        static char encoded[3]; // Buffer to hold the encoded characters
        encoded[0] = 127; // First character
        encoded[1] = outchar - 78; // Second character
        encoded[2] = '\0'; // Null-terminate the string
    }else {
       encoded[0] = outchar; // Assign the character
       encoded[1] = '\0'; // Null-terminate the string
    }
    return encoded;
}

void sendUdp() {
    if (send(sock, content, strlen(content), 0) == SOCKET_ERROR) {
        printf("send() error\n");
        closesocket(sock);
        exit(1);
    }
}

void formatTime() {
    time_t now = time(NULL);
    struct tm *info = localtime(&now);
    // Format the time as HH:MM:Day
    // %H: Hour (24-hour clock) as a zero-padded decimal number (00-23)
    // %M: Minute as a zero-padded decimal number (00-59)
    // %A: Full weekday name (e.g., "Sunday", "Monday")
    strftime(strTime, sizeof(strTime), "%H:%M:%A", info);
}

void getDates() {
   time_t now = time(NULL);
   struct tm localTime = *localtime(&now);

   currentYear = localTime.tm_year+1900;

   struct tm *xmas = localtime(&now);
   xmas->tm_mon = 11;
   xmas->tm_mday= 25;
   if (localTime.tm_mday > 25 && localTime.tm_mon ==11) { // MERRY CHRISTMAS!!
      xmas->tm_year = localTime.tm_year+1;
   }
   double seconds = difftime(mktime(xmas),now);
   double days=seconds/86400;

   dateDec25 = days;
   printf("%g days\n", days);

}


//##############################################################################
// INITITIALISE PARAMETERS AND OPEN COM PORT
//##############################################################################

// Can't use a sub routine due to use of the DATA command in
// the dataInit procedure.. so goto and "goto as a return"

//Connect to host
struct sockaddr_in sa;

int main(int argc, char *argv[]) {

	int days = daysUntilChristmas();
    printf("There are %d days until Christmas.\n", days);
    return 0;

    int delay = 50;
    double starttime = time(NULL);
    double lastmark = time(NULL);
    int repeatCount = 0;

    //set the default rotation center
    int RXC = 23;
    int RYC = 7;
    int Rinc = 15;
    int Rto = 360;

    FILE *parameters;

    // Configure the UDP Port
    sa.sin_port = htons(8082);
    sa.sin_family = AF_INET;
    //sa.sin_addr.s_addr = inet_addr("192.168.42.102");
    sa.sin_addr.s_addr = inet_addr("192.168.4.1");

    dataInit();

    if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
        printf("WSAStartup failed!\n");
        exit(1);
    }

    //Create the UDP socket
    int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    //sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock == INVALID_SOCKET) {
        printf("Socket creation failed!\n");
        WSACleanup();
        exit(1);
    }

    if (connect(sock, (struct sockaddr*)&sa, sizeof(sa)) == SOCKET_ERROR) {
        printf("connect() error\n");
        closesocket(sock);
        exit(1);
    }

    parameters = fopen("triksc.dat", "r");

    if (parameters == NULL) {
        printf("\nFAILED to open TRIKSC.DAT\n");
        printf("Would you like to setup (y/n)? ");
        char decision = getc(stdin);
        printf("%c\n", decision);
        if (decision == 'y' || decision == 'Y') {
            fclose(parameters);
            setup();
        } else {
            closesocket(sock);
            exit(1);
        }
    } else {
        printf("TRIKSC.DAT FOUND\n");

        fgets(strComPort, sizeof(strComPort), parameters);
        strComPort[strcspn(strComPort, "\n")] = 0; // Remove newline
        strDelay[256];

        fgets(strDelay, sizeof(strDelay), parameters);
        panelCount = atoi(strDelay);

        fgets(strDelay, sizeof(strDelay), parameters);
        framedelay = atoi(strDelay);

        fgets(strDelay, sizeof(strDelay), parameters);
        hedgedelay = atoi(strDelay);

        fgets(strDelay, sizeof(strDelay), parameters);
        vedgedelay = atoi(strDelay);

        fgets(strDelay, sizeof(strDelay), parameters);
        rotdelay = atoi(strDelay);
	printf("DEBUG: file parsing completed");
    }

    extern int _CRT_glob;
    _CRT_glob = 0;
    
    printf("exe name= %s\n", argv[0]);

    //''''sComOpenCmd=strComPort+" 57600,n,8,1,cs0,ds0,cd0,rs";

    //''''comport=freefile;
    //''''Open Com sComOpenCmd As #comport;

    //''''If Err Then
    //''''        printf("FAILED to open Comport!!! %s\n", strComPort);
    //''''        printf("### IS IT ALREADY OPEN BY ANOTHER COMMAND??? #####\n");
    //''''        sleep(5000);
    //''''        exit(1);
    //''''Else
    //''''        printf("COM port OK (%s)\n", strComPort);
    //''''        printf("F_delay=%d H_delay=%d\n", framedelay, hedgedelay);
    //''''        printf("V_delay=%d R_delay=%d\n", vedgedelay, rotdelay);
    //''''        printf("Number of Panels=%d\n", panelCount);
    //''''End If

    int nocommand = 1;
    int use_script = 0;

    //set the default rotation settings
    RXC = 23;
    RYC = 7;
    Rinc = 15;
    Rto = 360;

    // Now process the commands, one by one


  //##############################################################################
  //Interpret next parameter
  //##############################################################################

   char file_data[100];
   char *argv_ptr;
   for (int arg_count = 0; arg_count < argc; arg_count++) {
      if (use_script == 1) {
        if (feof(scriptfile)) {
            fclose(scriptfile);
            no_params(); // Does not return 
        } else {
            fscanf(scriptfile, "%s", file_data); // LabRat - put range checking eventually
	    argv_ptr = &file_data[0];
        }
      } else {
          argv_ptr = argv[arg_count];
      }

      if (strlen(argv_ptr) == 0) {
        if (argc == 1) {
            printCmdHelp();
        } else if (argc == 2 && nocommand == 1) {
            printf("\nSwitching to script file: %s", inputfile);
            use_script = 1;
	    strcpy(scriptfilename, inputfile);
            scriptfile = fopen(scriptfilename, "r");
            if (scriptfile == NULL) {
                printf("FAILED to open SCRIPT FILE !!!\n");
                sleep(1);
                closesocket(sock);
                exit(1);
            } else {
                printf("Script File Opened OK\n");
                continue;
            }
        }
        no_params(); // does not return
      }

      //now check to see if its a filename
      if (argv_ptr[0] != '#') {
        strcpy(inputfile, argv_ptr);
        continue;
      } else {
	char *s = argv_ptr;
	while (*s) {
           *s = toupper(*s);
	   s++;
	}
	strcpy(cmd, argv_ptr);
        printf("COMMAND: %s\n", cmd);
        nocommand = 0;
      }
    //now we have a command
    // MOVE COMMAND PARSERS HERE
    printf("Start Adding Commands Here");

    //#############################################################################
    // PROCESS COMMANDS that don't need file interpretation
    //#############################################################################
    if (strcmp(cmd, "#DIS") == 0) {
        diagFlag = 1;
        //screenres(800, 600, 8);
    } else if (strcmp(cmd, "#CLS") == 0) {
        strcpy(content,(char[]){126, 158, 126, 166, 126, 198,0});
        //Print(comport, content);
        sendUdp();
    } else if (strcmp(cmd, "#BLK") == 0) {
        strcpy(content,(char[]){126, 152,0});
        //Print(comport, content);
        sendUdp();
    } else if (strcmp(cmd, "#SHW") == 0) {
        strcpy(content,(char[]){126, 159,0});
        //Print(comport, content);
        sendUdp();
    } else if (strncmp(cmd, "#DLY", 4) == 0) {
        sleep(atoi(cmd + 5));
    } else if (strncmp(cmd, "#FDL", 4) == 0) {
        framedelay = atoi(cmd + 5);
    } else if (strncmp(cmd, "#VDL", 4) == 0) {
        vedgedelay = atoi(cmd + 5);
    } else if (strncmp(cmd, "#HDL", 4) == 0) {
        hedgedelay = atoi(cmd + 5);
    } else if (strncmp(cmd, "#NP", 3) == 0) {
        panelCount = atoi(cmd + 4);
    } else if (strncmp(cmd, "#RDL", 4) == 0) {
        rotdelay = atoi(cmd + 5);
    } else if (strncmp(cmd, "#RTO", 4) == 0) {
        Rto = atoi(cmd + 5);
    } else if (strncmp(cmd, "#RINC", 5) == 0) {
        Rinc = atoi(cmd + 6);
    } else if (strncmp(cmd, "#STA", 4) == 0) {
        strcpy(content,(char[]){126, 129,0});
        //Print(comport, content);
        sendUdp();
    } else if (strncmp(cmd, "#TSD", 4) == 0) {
	strncpy(file_id, cmd+5, 2);
        //file_id = strndup(cmd + 5, 2);
        strcpy(content, (char[]){126, 130,0});
        //strcat(content, file_id);
        //Print(comport, content);
        sendUdp();
        printf("file ID: %s\n", file_id);
    } else if (strcmp(cmd, "#TIME") == 0) {
        printf("Time elapsed = %.3f seconds\n", time(NULL) - starttime);
        printf("PRESS ANY KEY TO CONTINUE\n");
        getc(stdin);
    } else if (strncmp(cmd, "#RPT", 4) == 0) {
        if (repeatCount < atoi(cmd + 5)) {
            repeatCount++;
            argc = 1;
            if (use_script == 1) {
                fclose(scriptfile);
                scriptfile = fopen(scriptfilename, "r");
                if (scriptfile == NULL) {
                    printf("FAILED to open SCRIPT FILE !!!\n");
                    closesocket(sock);
                    usleep(1);
                    exit(1);
                } else {
                    printf("Script File ReOpened OK\n");
                }
            }
        }
    } else if (strncmp(cmd, "#LOOP", 5) == 0) {
        if (getc(stdin) == '\0') {
            argc = 1;
            if (use_script == 1) {
                fclose(scriptfile);
                scriptfile = fopen(scriptfilename, "r");
                if (scriptfile == NULL) {
                    printf("FAILED to open SCRIPT FILE !!!\n");
                    closesocket(sock);
                    usleep(1);
                    exit(1);
                } else {
                    printf("Script File ReOpened OK\n");
                }
            }
        }
    } else if (strncmp(cmd, "#RYC", 4) == 0) {
        RYC = atoi(cmd + 5);
    } else if (strncmp(cmd, "#RXC", 4) == 0) {
        RXC = atoi(cmd + 5);
    } else if (strncmp(cmd, "#ROT", 4) == 0) {
        if (strncmp(cmd, "#ROTL", 5) == 0) {
            // direction = -1
            handleRotation(-1);
        } else {
            // direction = 1
            handleRotation(1);
        }
    } else if (strncmp(cmd, "#CLK", 4) == 0) {
        gTimeout = time(NULL) + atoi(cmd + 5);
        strcpy(content,(char[]){126, 142,0}); // clear the screen panel 1
        //Print(comport, content);
        sendUdp();

        memset(Panel_1, 0, sizeof(Panel_1));
        Panel_1[5 * 48 + 14] = 1;  // display the colons
        Panel_1[6 * 48 + 14] = 1;
        Panel_1[10 * 48 + 14] = 1;
        Panel_1[11 * 48 + 14] = 1;
        Panel_1[5 * 48 + 15] = 1;
        Panel_1[6 * 48 + 15] = 1;
        Panel_1[10 * 48 + 15] = 1;
        Panel_1[11 * 48 + 15] = 1;
    
        Panel_1[5 * 48 + 31] = 1;
        Panel_1[6 * 48 + 31] = 1;
        Panel_1[10 * 48 + 31] = 1;
        Panel_1[11 * 48 + 31] = 1;
        Panel_1[5 * 48 + 32] = 1;
        Panel_1[6 * 48 + 32] = 1;
        Panel_1[10 * 48 + 32] = 1;
        Panel_1[11 * 48 + 32] = 1;
    
        do {
	    formatTime(); // Updates the GLOBAL strTime - move to 
            // Retrieve formatted time string, and parse H, M, D
	    int lhr, lmin, lsec;

	    if (sscanf(strTime, "%d:%d:%d",&lhr, &lmin, &lsec) == 3) {
                H_t = lhr/10;
                if (H_t == 0) {
                    H_t = 10;
                }
		H_u = lhr%10;
		M_t = lmin/10;
		M_u = lmin%10;
		S_t = lsec/10;
		S_u = lsec%10;
	    }else {
	       printf("WARN: Could not parse time string\n");
	    }
	    /* ORIGINAL CODE 
            if (H_t == 0) {
                H_t = 10;
            }
            H_u = atoi(strndup(strTime + 1, 1));
            M_t = atoi(strndup(strTime + 4, 1));
            M_u = atoi(strndup(strTime + 5, 1));
            S_t = atoi(strndup(strTime + 7, 1));
            S_u = atoi(strndup(strTime + 8, 1));
	    */
    
            if (S_u == previousS_u) {
                goto skipClockLoop;
            }
            printf("Time: %s\n", strTime);
            previousS_u = S_u;
    
            // hours_tens
            start = 3 * 48 + 0;
            bitCount = 0;
            for (rowCount = 0; rowCount <= 10; rowCount++) {
                for (columnCount = 0; columnCount <= 6; columnCount++) {
                    Panel_1[3 * 48 + (rowCount * 48) + columnCount] = clockDigits[H_t][bitCount];
                    Panel_1[3 * 48 + 7 + (rowCount * 48) + columnCount] = clockDigits[H_u][bitCount];
                    Panel_1[3 * 48 + 17 + (rowCount * 48) + columnCount] = clockDigits[M_t][bitCount];
                    Panel_1[3 * 48 + 24 + (rowCount * 48) + columnCount] = clockDigits[M_u][bitCount];
                    Panel_1[3 * 48 + 34 + (rowCount * 48) + columnCount] = clockDigits[S_t][bitCount];
                    Panel_1[3 * 48 + 41 + (rowCount * 48) + columnCount] = clockDigits[S_u][bitCount];
                    bitCount++;
                }
            }
    
	    strcpy(content,(char[]){126, 141,0}); // send if out panel 1
            // output stream
            for (columnCount = 95; columnCount >= 0; columnCount--) {
                // Bit manipulation: from the bit array build BYTE representing value in column of ROW0
                byte1 = Panel_1[columnCount * 8 + 0] + 2 * Panel_1[columnCount * 8 + 1] + 4 * Panel_1[columnCount * 8 + 2] + 8 * Panel_1[columnCount * 8 + 3] + 16 * Panel_1[columnCount * 8 + 4] + 32 * Panel_1[columnCount * 8 + 5] + 64 * Panel_1[columnCount * 8 + 6] + 128 * Panel_1[columnCount * 8 + 7];
                // Escape Encoding the byte if necessary
                strcat(content, escapeEncode(byte1));
            }
            //Print(comport, content);
            sendUdp();
    skipClockLoop:
            gActualTime = time(NULL);
        } while (gActualTime <= gTimeout);
    } else if (strncmp(cmd, "#CAL", 4) == 0) {
        gTimeout = time(NULL) + atoi(cmd + 5);
	
        strcpy(content,(char[]){126, 149,0});
        for (byteCount = 95; byteCount >= 0; byteCount--) {
            byte1 = panel1[byteCount]; // Use the saved FRAME template
            // Escape encoding
            strcat(content, escapeEncode(byte1));
        }
    
        strcat(content, (char[]){126, 141,0});
        for (byteCount = 95; byteCount >= 0; byteCount--) {
            byte1 = panel2[byteCount];
            strcat(content, escapeEncode(byte1));
        }
    
        //put(comport, content);
        sendUdp();
    
        do {
			// See new code at lines 140=180 and 257-259
		/* TO FIX - 
            currentTimeDate = time(NULL);
            strDaysRemaining = itoa(int(dateDec25) - int(currentTimeDate) - 1);
            strTimeRemaining = format(dateDec25 - currentTimeDate, "hh:mm:ss");
            if (atoi(strDaysRemaining) == -1) {
                strDaysRemaining = "000";
            } */
		// DEBUG TEMP CODE
            strcpy(strDaysRemaining,"000");
	    strcpy(strTimeRemaining,"00:00:42");
		// DEBUG TEMP CODE
	
            printf("days: %s\n", strDaysRemaining);
            printf("time: %s\n", strTimeRemaining);

		/* TO FIX - 
            d_h = atoi(strDaysRemaining) / 100;
            d_t = atoi(strndup(strDaysRemaining + strlen(strDaysRemaining) - 1, 1));
            d_u = atoi(strndup(strDaysRemaining + strlen(strDaysRemaining), 1));

            H_t = atoi(strndup(strTimeRemaining, 1));
            H_u = atoi(strndup(strTimeRemaining + 1, 1));
            M_t = atoi(strndup(strTimeRemaining + 4, 1));
            M_u = atoi(strndup(strTimeRemaining + 5, 1));
            S_t = atoi(strndup(strTimeRemaining + 7, 1));
            S_u = atoi(strndup(strTimeRemaining + 8, 1));

            bitCount = 0;
            for (rowCount = 0; rowCount <= 6; rowCount++) { // rows
                for (columnCount = 0; columnCount <= 4; columnCount++) { // clock digit columns
                    // Days (hundred::tens::units)
                    Panel_2[9 * 48 + 3 + (rowCount * 48) + columnCount] = digits[d_h][bitCount];
                    Panel_2[9 * 48 + 9 + (rowCount * 48) + columnCount] = digits[d_t][bitCount];
                    Panel_2[9 * 48 + 15 + (rowCount * 48) + columnCount] = digits[d_u][bitCount];
                    // Hours (tens::units)
                    Panel_2[9 * 48 + 30 + (rowCount * 48) + columnCount] = digits[h_t][bitCount];
                    Panel_2[9 * 48 + 36 + (rowCount * 48) + columnCount] = digits[h_u][bitCount];
                    // Minutes (tens::units)
                    Panel_1[9 * 48 + 3 + (rowCount * 48) + columnCount] = digits[m_t][bitCount];
                    Panel_1[9 * 48 + 9 + (rowCount * 48) + columnCount] = digits[m_u][bitCount];
                    // Seconds (tens::units)
                    Panel_1[9 * 48 + 27 + (rowCount * 48) + columnCount] = digits[s_t][bitCount];
                    Panel_1[9 * 48 + 33 + (rowCount * 48) + columnCount] = digits[s_u,][itCount];
                    bitCount++;
                }
            }

            strcpy(content,(char[]){126, 149,0});
            // Escape encoding the output stream (panel 1)
            for (byteCount = 95; byteCount >= 0; byteCount--) {
                byte1 =     Panel_1[byteCount * 8 + 0] + 
			2 * Panel_1[byteCount * 8 + 1] +
		       	4 * Panel_1[byteCount * 8 + 2] +
		       	8 * Panel_1[byteCount * 8 + 3] +
		       	16 * Panel_1[byteCount * 8 + 4] +
		       	32 * Panel_1[byteCount * 8 + 5] +
		       	64 * Panel_1[byteCount * 8 + 6] +
		       	128 * Panel_1[byteCount * 8 + 7];
                strcat(content, escapeEncode(byte1));
            }
	    */
            //Print(comport, content);
            sendUdp();

            strcpy(content,(char[]){126, 141,0});
            for (byteCount = 95; byteCount >= 0; byteCount--) {
                byte1 =     Panel_2[byteCount * 8 + 0] +
		       	2 * Panel_2[byteCount * 8 + 1] +
		       	4 * Panel_2[byteCount * 8 + 2] +
		       	8 * Panel_2[byteCount * 8 + 3] +
		       	16 * Panel_2[byteCount * 8 + 4] +
		       	32 * Panel_2[byteCount * 8 + 5] +
		       	64 * Panel_2[byteCount * 8 + 6] +
		       	128 * Panel_2[byteCount * 8 + 7];
                strcat(content, escapeEncode(byte1));
            }
            //Print(comport, content);
            sendUdp();

            if (H_t + H_u + M_t + M_u + S_t + S_u == 0) {
                break;
            }
            // check if timeout has occurred
            gActualTime = time(NULL);
        } while (gActualTime <= gTimeout);
    }// else #CAL
  } // for

  // ##############################################################################
  // Now interpret a VIXEN LEDTRIKS FILE
  // ##############################################################################

    printf("INTERPRETING VIXEN FILE : %s\n", inputfile);
    FILE *infile;
    infile = fopen(inputfile, "r");
    if (infile == NULL) {
        printf("FAILED to open LEDTRIKS FILE !!!\n");
        printf("(Check the syntax of your command line)\n");
        closesocket(sock);
        usleep(1);
        exit(1);
    } else {
        printf("LEDTRIKS File Opened OK\n");
    }


}

#ifdef DONTUSE
Find_Frames:
fscanf(infile, "%s", rd);
if (feof(infile)) goto byebye;

if (strstr(rd, "Frames") != NULL) {
    printf("Found Frame Data\n");
    goto GetData;
} else {
    goto Find_Frames;
}

GetData:
fscanf(infile, "%s", rd);
if (feof(infile)) {
    goto ByeBye;
}

if (strstr(rd, "Frame length") != NULL) {
    // process the next frame

    memset(panel1, 0, sizeof(panel1));
    memset(panel2, 0, sizeof(panel2));
    memset(panel3, 0, sizeof(panel3));
    memset(panel4, 0, sizeof(panel4));

    // Basically - the input command only seems to read 4kbytes. For big frames this
    // isn't enough... so need to read multiple lines and concatenate them
    // empirically ..the first line of the new read seems to have the last char of
    // the last read... so for now just strip the first char without knowing why...

    // keep concatenating until we have the end of frame
    while (strstr(rd, "/Frame") == NULL) {
        fscanf(infile, "%s", rextra);
        rd = strcat(rd, rextra + 1);
    }

    Z = strstr(rd, "=") - rd + 2;
    // could look for frame delay
    z = strstr(rd, ">") - rd + 1;

    sflag = 0;
    pflag = 0;
    Panel_loop:
    // timertest=time(NULL)

    b64quad = substr(rd, z, 4);
    if (strstr(b64quad, "<") != NULL) goto framedone;

    // Now just check if there's an ugly boundary we need to handle
    if (strlen(b64quad) != 4) {
        fscanf(infile, "%s", rd);
        z = 1;
        b64quad = substr(rd, z, 4);
        if (strstr(b64quad, "<") != NULL) goto framedone;
    }

    a1 = strchr(b64decode, b64quad[0]) - b64decode - 1;
    a2 = strchr(b64decode, b64quad[1]) - b64decode - 1;
    a3 = strchr(b64decode, b64quad[2]) - b64decode - 1;
    a4 = strchr(b64decode, b64quad[3]) - b64decode - 1;

    if (a3 < 0) a3 = 0;
    if (a4 < 0) a4 = 0;

    if (sflag == 0) {
        b1 = ((a1 << 2) & 0xFC) + ((a2 >> 4) & 0x03);
        b2 = ((a2 << 4) & 0xF0) + ((a3 >> 2) & 0x0F);
        b3 = ((a3 << 6) & 0xC0) + (a4 & 0x3F);
        sflag = 1;
    } else if (sflag == 1) {
        b4 = ((a1 << 2) & 0xFC) + ((a2 >> 4) & 0x03);
        if (b1 < 16) {
            if (b3 < 48) {
                // PANEL 1
                pixel = b1 * 48 + b3;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel1[pixelbyte] |= (1 << pixelpos);
            } else if ((b3 - 48) < 48) {
                // PANEL 2
                pixel = b1 * 48 + b3 - 48;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel2[pixelbyte] |= (1 << pixelpos);
            } else {
                // Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
                // Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
            }
        } else if (b1 < 32) {
            if (b3 < 48) {
                // PANEL 3
                pixel = (b1 - 16) * 48 + b3;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel3[pixelbyte] |= (1 << pixelpos);
            } else if ((b3 - 48) < 48) {
                // PANEL 4
                pixel = (b1 - 16) * 48 + b3 - 48;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel4[pixelbyte] |= (1 << pixelpos);
            } else {
                // Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
                // Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
            }
        } else {
            // Print "ROW EXCEPTION - DATA IS > 2 FRAME TALL"
            // Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
        }
        b1 = ((a2 << 4) & 0xF0) + ((a3 >> 2) & 0x0F);
        b2 = ((a3 << 6) & 0xC0) + (a4 & 0x3F);
        sflag = 2;
    } else if (sflag == 2) {
        b3 = ((a1 << 2) & 0xFC) + ((a2 >> 4) & 0x03);
        b4 = ((a2 << 4) & 0xF0) + ((a3 >> 2) & 0x0F);
        if (b1 < 16) {
            if (b3 < 48) {
                // PANEL 1
                pixel = b1 * 48 + b3;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel1[pixelbyte] |= (1 << pixelpos);
            } else if ((b3 - 48) < 48) {
                // PANEL 2
                pixel = b1 * 48 + b3 - 48;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel2[pixelbyte] |= (1 << pixelpos);
            } else {
                // Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
                // Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
            }
        } else if (b1 < 32) {
            if (b3 < 48) {
                // PANEL 3
                pixel = (b1 - 16) * 48 + b3;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel3[pixelbyte] |= (1 << pixelpos);
            } else if ((b3 - 48) < 48) {
                // PANEL 4
                pixel = (b1 - 16) * 48 + b3 - 48;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel4[pixelbyte] |= (1 << pixelpos);
            } else {
                // Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
                // Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
            }
        } else {
            // Print "ROW EXCEPTION - DATA IS > 2 FRAME TALL"
            // Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
        }
        b1 = ((a3 << 6) & 0xC0) + (a4 & 0x3F);
        sflag = 3;
    } else if (sflag == 3) {
        b2 = ((a1 << 2) & 0xFC) + ((a2 >> 4) & 0x03);
        b3 = ((a2 << 4) & 0xF0) + ((a3 >> 2) & 0x0F);
        b4 = ((a3 << 6) & 0xC0) + (a4 & 0x3F);
        if (b1 < 16) {
            if (b3 < 48) {
                // PANEL 1
                pixel = b1 * 48 + b3;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel1[pixelbyte] |= (1 << pixelpos);
            } else if ((b3 - 48) < 48) {
                // PANEL 2
                pixel = b1 * 48 + b3 - 48;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel2[pixelbyte] |= (1 << pixelpos);
            } else {
                // Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
                // Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
            }
        } else if (b1 < 32) {
            if (b3 < 48) {
                // PANEL 3
                pixel = (b1 - 16) * 48 + b3;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel3[pixelbyte] |= (1 << pixelpos);
            } else if ((b3 - 48) < 48) {
                // PANEL 4
                pixel = (b1 - 16) * 48 + b3 - 48;
                pixelbyte = pixel / 8;
                pixelpos = pixel - pixelbyte * 8;
                panel4[pixelbyte] |= (1 << pixelpos);
            } else {
                // Print "COLUMN EXCEPTION - DATA IS > 2 FRAMES WIDE"
                // Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
            }
        } else {
            // Print "ROW EXCEPTION - DATA IS > 2 FRAME TALL"
            // Print "(NOT SUPPORTED BY THIS VERSION OF LTC)"
        }
        sflag = 0;
    }

    z += 4;
    testlen = strlen(rd);
    if (z > testlen) {
        fscanf(infile, "%s", rd);
        z = 1;
    }
    goto Panel_loop;

//###########################################################################
// Now we have a frame Process the command that handles it
//###########################################################################
Framedone:

if (strcmp(cmd, "#RL") == 0) {
    // To scroll text Right to Left just take the edge of each of the frames and shift it

    p1byte1 = 0;  // 2 bytes that contain edge of frame to be rotated in
    p1byte2 = 0;
    p2byte1 = 0;
    p2byte2 = 0;
    p3byte1 = 0;
    p3byte2 = 0;
    p4byte1 = 0;
    p4byte2 = 0;

    Z = 128;
    for (rowCount = 1; rowCount <= 8; rowCount++) {
        yr = (rowCount * 6) - 1;  // byte position end of row(s) 1 to 8

        if (bit(panel1[yr], 7) == -1) {  // right edge of frame rows 0 to 7
            p1byte1 += Z;
        }

        if (bit(panel2[yr], 7) == -1) {  // right edge of frame rows 0 to 7
            p2byte1 += Z;
        }

        if (bit(panel3[yr], 7) == -1) {  // right edge of frame rows 0 to 7
            p3byte1 += Z;
        }

        if (bit(panel4[yr], 7) == -1) {  // right edge of frame rows 0 to 7
            p4byte1 += Z;
        }

        yr = ((rowCount + 8) * 6) - 1;  // byte position end of rows 8-15

        if (bit(panel1[yr], 7) == -1) {
            p1byte2 += Z;
        }

        if (bit(panel2[yr], 7) == -1) {
            p2byte2 += Z;
        }

        if (bit(panel3[yr], 7) == -1) {
            p3byte2 += Z;
        }

        if (bit(panel4[yr], 7) == -1) {
            p4byte2 += Z;
        }

        Z /= 2;
    }

    // Print THE 2 EDGE BYTES
    memset(content, 0, sizeof(content));
    if (1) {
        content += chr(126) + chr(137);  // PANEL 2 RIGHT EDGE
        // Escape encoding
        content += escapeEncode(p1byte1);
        // Escape encoding
        content += escapeEncode(p1byte2);
    }
    if (1) {
        content += chr(126) + chr(145);  // PANEL 2 RIGHT EDGE
        // Escape encoding
        content += escapeEncode(p2byte1);
        // Escape encoding
        content += escapeEncode(p2byte2);
    }
    if (1) {
        content += chr(126) + chr(161);  // PANEL 2 RIGHT EDGE
        // Escape encoding
        content += escapeEncode(p3byte1);
        // Escape encoding
        content += escapeEncode(p3byte2);
    }
    if (1) {
        content += chr(126) + chr(193);  // PANEL 2 RIGHT EDGE
        // Escape encoding
        content += escapeEncode(p4byte1);
        // Escape encoding
        content += escapeEncode(p4byte2);
    }
    put(comport, "", content);
    sendUdp();

    tdelayms = (time(NULL) - lastmark) * 1000;
    delay = hedgedelay - tdelayms;
    if (delay > 0) {
        usleep(delay*1000);
    } else {
        printf("!");
    }
    lastmark = time(NULL);

} else if (strcmp(cmd, "#RR") == 0) {
    // To scroll text Left to Right

    p1byte1 = 0;  // 2 bytes that contain edge of frame to be rotated in
    p1byte2 = 0;
    p2byte1 = 0;
    p2byte2 = 0;
    p3byte1 = 0;
    p3byte2 = 0;
    p4byte1 = 0;
    p4byte2 = 0;
    Z = 128;
    for (rowCount = 1; rowCount <= 8; rowCount++) {

        yl = (rowCount - 1) * 6;  // byte position start of rows

        if (bit(panel1[yl], 7) == -1) {  // left edge of frame rows 0 to 7
            p1byte1 += Z;
        }

        if (bit(panel2[yl], 7) == -1) {  // left edge of frame rows 0 to 7
            p2byte1 += Z;
        }

        if (bit(panel3[yl], 7) == -1) {  // left edge of frame rows 0 to 7
            p3byte1 += Z;
        }

        if (bit(panel4[yl], 7) == -1) {  // left edge of frame rows 0 to 7
            p4byte1 += Z;
        }

        // byte position end of rows 8-15
        yl = (rowCount - 1 + 8) * 6;  // byte position start of rows 8-15
        if (bit(panel1[yl], 7) == -1) {  // left edge of frame rows 8-15
            p1byte2 += Z;
        }
        if (bit(panel2[yl], 7) == -1) {  // left edge of frame rows 8-15
            p2byte2 += Z;
        }

        if (bit(panel3[yl], 7) == -1) {  // left edge of frame rows 8-15
            p3byte2 += Z;
        }

        if (bit(panel4[yl], 7) == -1) {  // left edge of frame rows 8-15
            p4byte2 += Z;
        }

        Z /= 2;
    }
    // Print THE 2 EDGE BYTES
    memset(content, 0, sizeof(content));
    if (1) {
        content += chr(126) + chr(138);  // PANEL 2 RIGHT EDGE
        // Escape encoding
        content += escapeEncode(p1byte1);
        content += escapeEncode(p1byte2);
    }
    if (1) {
        content += chr(126) + chr(146);  // PANEL 2 RIGHT EDGE
        // Escape encoding
        content += escapeEncode(p2byte1);
        content += escapeEncode(p2byte2);
    }
    if (1) {
        content += chr(126) + chr(162);  // PANEL 2 RIGHT EDGE
        // Escape encoding
        content += escapeEncode(p3byte1);
        content += escapeEncode(p3byte2);
    }
    if (1) {
        content += chr(126) + chr(194);  // PANEL 2 RIGHT EDGE
        content += escapeEncode(p4byte1);
        content += escapeEncode(p4byte2);
    }

    put(comport, "", content);
    sendUdp();

    tdelayms = (time(NULL) - lastmark) * 1000;
    delay = hedgedelay - tdelayms;
    if (delay > 0) {
        usleep(delay*1000);
    } else {
        printf("!");
    }
    lastmark = time(NULL);
}
 elseif (strcmp(cmd, "#RD") == 0) {
    memset(content, 0, sizeof(content));
     if (1) {
         strcat(content, "\x7E\x8C");
         for (rowCount = 5; rowCount >= 0; rowCount--) {
             byte1 = panel1[rowCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     if (1) {
         strcat(content, "\x7E\x94");
         for (rowCount = 5; rowCount >= 0; rowCount--) {
             byte1 = panel2[rowCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     if (1) {
         strcat(content, "\x7E\xA4");
         for (rowCount = 5; rowCount >= 0; rowCount--) {
             byte1 = panel3[rowCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     if (1) {
         strcat(content, "\x7E\xC4");
         for (rowCount = 5; rowCount >= 0; rowCount--) {
             byte1 = panel4[rowCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     put(comport, content);
     sendUdp();
     tdelayms = (time(NULL) - lastmark) * 1000;
     delay = vedgedelay - tdelayms;
     if (delay > 0) {
         usleep(delay*1000);
     }
     lastmark = time(NULL);
 } else if (strcmp(cmd, "#RU") == 0) {
     memset(content, 0, sizeof(content));
     if (1) {
         strcat(content, "\x7E\x8B");
         for (byteCount = 95; byteCount >= 90; byteCount--) {
             byte1 = panel1[byteCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     if (1) {
         strcat(content, "\x7E\x93");
         for (byteCount = 95; byteCount >= 90; byteCount--) {
             byte1 = panel2[byteCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     if (1) {
         strcat(content, "\x7E\xA3");
         for (byteCount = 95; byteCount >= 90; byteCount--) {
             byte1 = panel3[byteCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     if (1) {
         strcat(content, "\x7E\xC3");
         for (byteCount = 95; byteCount >= 90; byteCount--) {
             byte1 = panel4[byteCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     put(comport, content);
     sendUdp();
     tdelayms = (time(NULL) - lastmark) * 1000;
     delay = vedgedelay - tdelayms;
     if (delay > 0) {
         usleep(delay*1000);
     } else {
         printf("!");
     }
     lastmark = time(NULL);
 } else if (strcmp(cmd, "#FRM") == 0) {
     memset(content,0, sizeof(content));
     // Panel 3 (165)
     if (panelCount > 2) {
         strcat(content, "\x7E\xA5");
         for (byteCount = 95; byteCount >= 0; byteCount--) {
             byte1 = panel3[byteCount];
             strcat(content, escapeEncode(byte1));
         }
     } else { // just in case we're scrolling....
         strcat(content, "\x7E\xA6"); // clear panel 3
     }
     // Panel 1 141
     if (1) { // always do panel 1
         strcat(content, "\x7E\x89");
         for (byteCount = 95; byteCount >= 0; byteCount--) {
             byte1 = panel1[byteCount];
            strcat(content, escapeEncode(byte1));
         }
     }
     // Panel 4 197
     if (panelCount > 3) {
         strcat(content, "\x7E\xC5");
         for (byteCount = 95; byteCount >= 0; byteCount--) {
             byte1 = panel4[byteCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     // Panel 2
     if (panelCount > 1) {
         strcat(content, "\x7E\x95");
         for (byteCount = 95; byteCount >= 0; byteCount--) {
             byte1 = panel2[byteCount];
             strcat(content, escapeEncode(byte1));
         }
     }
     put(comport, content);
     sendUdp();
     tdelayms = (time(NULL) - lastmark) * 1000;
     delay = framedelay - tdelayms;
     if (delay > 0) {
         for (x = 1; x <= delay / 2; x++) {
             usleep(2000);
         }
     } else {
         // printf("!");
     }
     lastmark = time(NULL);
 } else if (strncmp(cmd, "#W", 2) == 0) {
     file_id = cmd + 2;
     file_id = strcat(file_id, ".dat");
     if (file_is_open == 0) {
         remove(file_id);
         outfile = fopen(file_id, "a");
         if (outfile == NULL) {
             printf("Output file cannot be opened");
             usleep(1);
             exit(1);
         } else {
             printf("Output file: %s opened OK", file_id);
             file_is_open = 1;
         }
     }
     memset(content,0,sizeof(content));
     // Panel 3 (165)
     strcat(content, "\x7E\xA5");
     for (x = 95; x >= 0; x--) {
         byte1 = panel3[x];
         strcat(content, (char[]){byte1, '\0'});
     }
     // Panel 1 141
     strcat(content, "\x7E\x89");
     for (x = 95; x >= 0; x--) {
         byte1 = panel1[x];
         strcat(content, (char[]){byte1, '\0'});
     }
     // Panel 4 197
     strcat(content, "\x7E\xC5");
     for (x = 95; x >= 0; x--) {
         byte1 = panel4[x];
         strcat(content, (char[]){byte1, '\0'});
     }
     // Panel 2
     strcat(content, "\x7E\x95");
     for (x = 95; x >= 0; x--) {
         byte1 = panel2[x];
         strcat(content, (char[]){byte1, '\0'});
     }
     strcat(content, (char[120]){0});
     put(outfile, content);
 } else {
     printf("COMMAND NOT RECOGNISED: '%s'", cmd);
     usleep(1);
     goto byebye;
 }
 } // End of processing the frame
 
// FRAME DIAGNOSTIC
 if (diagFlag == 1) {
     printf("\n");
     printf("------------------------------------------------------------------------------------------------");
     for (rowCount = 0; rowCount <= 15; rowCount++) {
         for (columnCount = 0; columnCount <= 5; columnCount++) {
             for (bitCount = 0; bitCount <= 7; bitCount++) {
                 if (bit(panel1[rowCount * 6 + columnCount], bitCount) == -1) {
                     printf("*");
                 } else {
                     printf(" ");
                 }
             }
         }
         for (columnCount = 0; columnCount <= 5; columnCount++) {
             for (bitCount = 0; bitCount <= 7; bitCount++) {
               if (bit(panel2[rowCount * 6 + columnCount], bitCount) == -1) {
                     printf("0");
                 } else {
                     printf(" ");
                 }
             }
         }
         printf(" |\n");
     }
     for (rowCount = 0; rowCount <= 15; rowCount++) {
         for (columnCount = 0; columnCount <= 5; columnCount++) {
             for (bitCount = 0; bitCount <= 7; bitCount++) {
                if (bit(panel3[rowCount * 6 + columnCount], bitCount) == -1) {
                     printf("X");
                 } else {
                     printf(" ");
                 }
             }
         }
         for (columnCount = 0; columnCount <= 5; columnCount++) {
             for (bitCount = 0; bitCount <= 7; bitCount++) {
                 if (bit(panel4[rowCount * 6 + columnCount], bitCount) == -1) {
                     printf("#");
                 } else {
                     printf(" ");
                 }
             }
         }
         printf(" |\n");
     }
     printf("------------------------------------------------------------------------------------------------");
     usleep(1);
 }
 No_Display:
 goto getdata;
#endif
//##########################################################################
// EXIT -OUR WORK HERE IS DONE
//##########################################################################

void ByeBye(){
    fclose(infile);
    if (file_is_open == 1) {
        fclose(outfile);
    }
}

void no_params(){
    fclose(comport);

    if (sock) {
        closesocket(sock);
    }
    exit(0);
}
//#########################################################################
// SETTING UP TRIKSC.DAT
//#########################################################################

void setup() {
    system("cls");
    printf("##################################\n");
    printf("TRIKS-C Setup\n");
    printf("TESTING FOR COMPORTS (COM1:-COM40)\n");
    printf("##################################\n");
    testPorts();
    printf("\n");

getPort:
    char theport[20];
    printf("SETUP: Enter the COM port you wish to use: eg 'COM1:' : ");
    scanf("%s", theport);
    
    if (strstr(strlwr(theport), "com") == NULL || strstr(theport, ":") == NULL) {
        printf(" THE FORMAT MUST BE 'COMx:' where x is your port number\n");
        goto getPort;
    }

    char theport1[50];
    sprintf(theport1, "%s 57600,n,8,1,cs0,ds0,cd0,rs", theport);
    FILE *fp = fopen(theport1, "r");
    
    if (fp == NULL) {
        printf("Port %s cannot be opened\n", theport);
        usleep(1000);
        exit(1);
    } else {
        fclose(fp);
        remove("triksc.dat");
        FILE *paramFile = fopen("triksc.dat", "w");
        
        if (paramFile == NULL) {
            printf("TRIKSC.DAT cannot be opened\n");
            usleep(1000);
            exit(1);
        }
        
        int panelCount, framedelay, hedgedelay, vedgedelay, rotdelay;
        printf("How many panels are on display (1-4): ");
        scanf("%d", &panelCount);
        printf("Now enter the frame delay (ms) (normal= 50): ");
        scanf("%d", &framedelay);
        printf("Now enter the horizontal edge scrolldelay (ms) (normal=50): ");
        scanf("%d", &hedgedelay);
        printf("Now enter the vertical edge scroll delay (ms) (normal= 100): ");
        scanf("%d", &vedgedelay);
        printf("Now enter the rotational delay (normal = 0): ");
        scanf("%d", &rotdelay);

        fprintf(paramFile, "%s\n", theport);
        fprintf(paramFile, "%d\n", panelCount);
        fprintf(paramFile, "%d\n", framedelay);
        fprintf(paramFile, "%d\n", hedgedelay);
        fprintf(paramFile, "%d\n", vedgedelay);
        fprintf(paramFile, "%d\n", rotdelay);
        fclose(paramFile);
    }
}
// ----------------------------------------------
// Subroutine to handle image rotation
// ----------------------------------------------
void handleRotation(int direction) {
#ifdef DONTUSE
    int DEGREE, rotpos;
    unsigned char panel1ROT[96]; // Frame buffers
    unsigned char panel2ROT[96];
    unsigned char panel3ROT[96];
    unsigned char panel4ROT[96];
    int byteIdx, rowIdx, bitIdx, xa;
    double RAD, radpos;
    double r, y1, x1;
    int pixel, pixel1, pixelbyte, pixelpos; //, rows, cols
    char content[256]; // Assuming a maximum length for content
    unsigned char byte1;

    rotpos = 0;
    for (DEGREE = rotpos; DEGREE <= rotpos + Rto * direction; DEGREE += Rinc * direction) {
        memset(panel1ROT, 0, sizeof(panel1ROT));
        memset(panel2ROT, 0, sizeof(panel2ROT));
        memset(panel3ROT, 0, sizeof(panel3ROT));
        memset(panel4ROT, 0, sizeof(panel4ROT));

        for (rowIdx = 0; rowIdx <= 15; rowIdx++) {
            for (byteIdx = 0; byteIdx <= 5; byteIdx++) {
                for (bitIdx = 0; bitIdx <= 7; bitIdx++) {
                    xa = byteIdx * 8 + bitIdx;

                    // Look at Panel 1 bits
                    radpos = atan2((rowIdx - RYC), (RXC - xa));
                    RAD = DEGREE * M_PI / 180;
                    if (bit(panel1[rowIdx * 6 + byteIdx], bitIdx) == -1) {
                        r = sqrt((xa - RXC) * (xa - RXC) + (rowIdx - RYC) * (rowIdx - RYC));
                        y1 = (int)(RYC - r * sin(RAD - radpos) + 0.5);
                        x1 = (int)(RXC - r * cos(RAD - radpos) + 0.5);
                        if (!(y1 < 0 || y1 > 15 || x1 < 0 || x1 > 47)) {
                            pixel1 = (int)(x1 + 0.5) + (int)(y1 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel1ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 0 || y1 > 15 || x1 < 48 || x1 > 95)) {
                            pixel1 = (int)(x1 - 48 + 0.5) + (int)(y1 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel2ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 16 || y1 > 31 || x1 < 0 || x1 > 47)) {
                            pixel1 = (int)(x1 + 0.5) + (int)(y1 - 16 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel3ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 16 || y1 > 31 || x1 < 48 || x1 > 95)) {
                            pixel1 = (int)(x1 - 48 + 0.5) + (int)(y1 - 16 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel4ROT[pixelbyte] += (1 << pixelpos);
                        }
                    }

                    // Look at Panel 2 bits (need to add 48 to xa)
                    radpos = atan2((rowIdx - RYC), (RXC - (xa + 48)));
                    RAD = DEGREE * M_PI / 180;
                    if (bit(panel2[rowIdx * 6 + byteIdx], bitIdx) == -1) {
                        r = sqrt((xa + 48 - RXC) * (xa + 48 - RXC) + (rowIdx - RYC) * (rowIdx - RYC));
                        y1 = (int)(RYC - r * sin(RAD - radpos) + 0.5);
                        x1 = (int)(RXC - r * cos(RAD - radpos) + 0.5);
                        if (!(y1 < 0 || y1 > 15 || x1 < 0 || x1 > 47)) {
                            pixel1 = (int)(x1 + 0.5) + (int)(y1 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel1ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 0 || y1 > 15 || x1 < 48 || x1 > 95)) {
                            pixel1 = (int)(x1 - 48 + 0.5) + (int)(y1 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel2ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 16 || y1 > 31 || x1 < 0 || x1 > 47)) {
                            pixel1 = (int)(x1 + 0.5) + (int)(y1 - 16 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel3ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 16 || y1 > 31 || x1 < 48 || x1 > 95)) {
                            pixel1 = (int)(x1 - 48 + 0.5) + (int)(y1 - 16 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel4ROT[pixelbyte] += (1 << pixelpos);
                        }
                    }

                    // Look at Panel 3 bits (need to add 16 to y)
                    radpos = atan2((rowIdx + 16 - RYC), (RXC - xa));
                    RAD = DEGREE * M_PI / 180;
                    if (bit(panel3[rowIdx * 6 + byteIdx], bitIdx) == -1) {
                        r = sqrt((xa - RXC) * (xa - RXC) + (rowIdx + 16 - RYC) * (rowIdx + 16 - RYC));
                        y1 = (int)(RYC - r * sin(RAD - radpos) + 0.5);
                        x1 = (int)(RXC - r * cos(RAD - radpos) + 0.5);
                        if (!(y1 < 0 || y1 > 15 || x1 < 0 || x1 > 47)) {
                            pixel1 = (int)(x1 + 0.5) + (int)(y1 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel1ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 0 || y1 > 15 || x1 < 48 || x1 > 95)) {
                            pixel1 = (int)(x1 - 48 + 0.5) + (int)(y1 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel2ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 16 || y1 > 31 || x1 < 0 || x1 > 47)) {
                            pixel1 = (int)(x1 + 0.5) + (int)(y1 - 16 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel3ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 16 || y1 > 31 || x1 < 48 || x1 > 95)) {
                            pixel1 = (int)(x1 - 48 + 0.5) + (int)(y1 - 16 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel4ROT[pixelbyte] += (1 << pixelpos);
                        }
                    }

                    // Look at Panel 4 bits (need to add 16 to y and 48 to xa)
                    radpos = atan2((rowIdx + 16 - RYC), (RXC - (xa + 48)));
                    RAD = DEGREE * M_PI / 180;
                    if (bit(panel4[rowIdx * 6 + byteIdx], bitIdx) == -1) {
                        r = sqrt((xa + 48 - RXC) * (xa + 48 - RXC) + (rowIdx + 16 - RYC) * (rowIdx + 16 - RYC));
                        y1 = (int)(RYC - r * sin(RAD - radpos) + 0.5);
                        x1 = (int)(RXC - r * cos(RAD - radpos) + 0.5);
                        if (!(y1 < 0 || y1 > 15 || x1 < 0 || x1 > 47)) {
                            pixel1 = (int)(x1 + 0.5) + (int)(y1 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel1ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 0 || y1 > 15 || x1 < 48 || x1 > 95)) {
                            pixel1 = (int)(x1 - 48 + 0.5) + (int)(y1 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel2ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 16 || y1 > 31 || x1 < 0 || x1 > 47)) {
                            pixel1 = (int)(x1 + 0.5) + (int)(y1 - 16 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel3ROT[pixelbyte] += (1 << pixelpos);
                        } else if (!(y1 < 16 || y1 > 31 || x1 < 48 || x1 > 95)) {
                            pixel1 = (int)(x1 - 48 + 0.5) + (int)(y1 - 16 + 0.5) * 48;
                            pixelbyte = (int)(pixel1 / 8);
                            pixelpos = pixel1 - (int)(pixel1 / 8) * 8;
                            panel4ROT[pixelbyte] += (1 << pixelpos);
                        }
                    }
                }
            }
        }
    }
    #endif
}
#ifdef DONTUSE
// FRAME DIAGNOSTIC
if (diagFlag == 1) {
    printf("\n");
    printf("------------------------------------------------------------------------------------------------\n");
    for (int rowIdx = 0; rowIdx <= 15; rowIdx++) {
        for (int byteIdx = 0; byteIdx <= 5; byteIdx++) {
            for (int bitIdx = 0; bitIdx <= 7; bitIdx++) {
                if (bit(panel1rot[rowIdx * 6 + byteIdx], bitIdx) == -1) {
                    printf("*");
                } else {
                    printf(" ");
                }
            }
        }

        for (int byteIdx = 0; byteIdx <= 5; byteIdx++) {
            for (int bitIdx = 0; bitIdx <= 7; bitIdx++) {
                if (bit(panel2rot[rowIdx * 6 + byteIdx], bitIdx) == -1) {
                    printf("0");
                } else {
                    printf(" ");
                }
            }
        }

        printf(" |\n");
    }

    for (int rowIdx = 0; rowIdx <= 15; rowIdx++) {
        for (int byteIdx = 0; byteIdx <= 5; byteIdx++) {
            for (int bitIdx = 0; bitIdx <= 7; bitIdx++) {
                if (bit(panel3rot[rowIdx * 6 + byteIdx], bitIdx) == -1) {
                    printf("X");
                } else {
                    printf(" ");
                }
            }
        }

        for (int byteIdx = 0; byteIdx <= 5; byteIdx++) {
            for (int bitIdx = 0; bitIdx <= 7; bitIdx++) {
                if (bit(panel4rot[rowIdx * 6 + byteIdx], bitIdx) == -1) {
                    printf("#");
                } else {
                    printf(" ");
                }
            }
        }
        printf(" |\n");
    }

    printf("------------------------------------------------------------------------------------------------\n");
    sleep(1); // Assuming sleep is in seconds
}

char content[1000] = ""; // Adjust size as necessary
content[strlen(content)] = '~';
content[strlen(content)] = (char)141;
// for (int byteIdx = 95; byteIdx >= 0; byteIdx--) // Commented out loop
for (int byteIdx = 0; byteIdx <= 95; byteIdx++) {
    byte1 = panel1rot[byteIdx];
    strcat(content, escapeEncode(byte1));
}

content[strlen(content)] = '~';
content[strlen(content)] = (char)149;
// for (int byteIdx = 95; byteIdx >= 0; byteIdx--) // Commented out loop
for (int byteIdx = 0; byteIdx <= 95; byteIdx++) {
    byte1 = panel2rot[byteIdx];
    strcat(content, escapeEncode(byte1));
}

content[strlen(content)] = '~';
content[strlen(content)] = (char)165;
// for (int byteIdx = 95; byteIdx >= 0; byteIdx--) // Commented out loop
for (int byteIdx = 0; byteIdx <= 95; byteIdx++) {
    byte1 = panel3rot[byteIdx];
    strcat(content, escapeEncode(byte1));
}

content[strlen(content)] = '~';
content[strlen(content)] = (char)197;
// for (int byteIdx = 95; byteIdx >= 0; byteIdx--) // Commented out loop
for (int byteIdx = 0; byteIdx <= 95; byteIdx++) {
    byte1 = panel4rot[byteIdx];
    strcat(content, escapeEncode(byte1));
}
put(comport, content);

tdelayms = (time(NULL) - lastmark) * 1000;
delay = rotdelay - tdelayms;
if (delay > 0) {
    usleep(delay * 1000); // Assuming delay is in milliseconds
}
lastmark = time(NULL);
rotpos = (rotpos + Rto * direction) % 360;
#endif

void printCmdHelp() {
    system("cls");
    printf("\n");
    printf("**************************************************************\n");
    printf("*   LEDTRIKS-C COMMAND LINE INTERFACE PROGRAM                *\n");
    printf("*   BETA RELEASE 05/9/10  Tim Wells      V0.11               *\n");
    printf("**************************************************************\n");
    printf("\n");
    printf(" This version can drive up to 2x2 TriksC/Ledtriks panels\n");
    printf("\n");
    printf("USAGE: LTC [filename] command [filename] [command] \n");
    printf("Commands:\n");
    printf("            #CLS     = clear screen\n");
    printf("            #BLK     = blank screen\n");
    printf("            #SHW     = Show screen\n");
    printf("            #RL      = Roll to Left (EDGE TRANSMISSION)\n");
    printf("            #RR      = Roll to Right\n");
    printf("            #RD      = Roll Down\n");
    printf("            #RU      = Roll UP\n");
    printf("            #FRM     = Whole Frame\n");
    printf("\n");
    printf("            #RPTxxxx = Repeat all xx times\n");
    printf("            #DLYnnnn = Wait nnnn ms\n");
    printf("            #LOOP    = Repeat until keypress\n");
    printf("\n");
    printf("            #FDLnnnn = Delay between Frames nnnn (ms)\n");
    printf("            #HDLnnnn = Delay for Horizontal scroll (ms)\n");
    printf("            #VDLnnnn = Delay for Vertical scroll (ms)\n");
    printf("            #RDLnnnn = Delay for Rotate (ms)\n");
    printf("            #NPx     = Number of active panels (1-4)\n");
    printf("\n");
    printf("            #TIME    = Display time to execute this file\n");
    printf("\n");
    printf("            #ROTR    = Rotate Clockwise (right)\n");
    printf("            #ROTL    = Rotate AntiClockwise (left)\n");
    printf("            #RINCnnn = Rotation Steps (nnn degrees)\n");
    printf("            #RTOnnn  = Rotation Angle (nnn degrees)\n");
    printf("            #RXCnnnn = Rotate Center X co-ord\n");
    printf("            #RYCnnnn = Rotate Center Y co-ord\n");
    printf("            \n");
    // printf("            #Wxx     = Write Triks SD data (file xx.dat)\n");
    // printf("            #TSDxx   = Order TRIKS SD to transmit file xx.*\n");
    printf("\n");
    printf("            #STA     = Send TRIKS-C to stand alone mode\n");
    printf("                       (until next command received)\n");
    printf("\n");
    printf("            #CALxxx  = Countdown timer (2 panels) xxx secs\n");
    printf("            #CLKxxx  = Clock (1 Panel) for xxx secs\n");
    printf("\n");
    printf(" PIX-C commands:\n");
    printf("            #P_PROGRAM = Assign PIXC board to a panel\n");
    printf("            #P_FSpff   = Display frame ff on panel p\n");
    printf("            #P_FWpff   = Store into frame ff on panel p\n");
    printf("\n");
    printf(" (OR LTC [filename] with no params to run a script file \n");
    printf("  with 1 command per line)\n");
    printf("\n");
    printf(" (TRIKSC.DAT stores comport in use- delete it to choose comport)\n");
    printf("\n");
    getc(stdin); // Wait for user input
}
void testPorts() {
#ifdef DONTUSE
     int idx;
     char strPortParam[50];
     int port;

     for (idx = 1; idx <= 40; idx++) {
         sprintf(strPortParam, "COM%d: 57600,n,8,1,cs0,ds0,cd0,rs", idx);
         if (open_com_port(strPortParam, &port) != 0) { // Placeholder for actual COM port opening function
             // nothing
         } else {
             printf("COM%d: EXISTS\n", idx);
             close_com_port(port); // Placeholder for actual COM port closing function
         }
     }
#endif
 }
void dataInit() {
   for (int byteCount = 0; byteCount < 96; byteCount++) {
       panel1[byteCount] = frame01[byteCount];
       for (int y = 0; y < 8; y++) {
           if (panel1[byteCount] & (1 << y)) {
               Panel_1[byteCount * 8 + y] = 1;
           }
       }
   }

   for (int byteCount = 0; byteCount < 96; byteCount++) {
       panel2[byteCount] = frame02[byteCount];
       for (int y = 0; y < 8; y++) {
           if (panel2[byteCount] & (1 << y)) {
               Panel_2[byteCount * 8 + y] = 1;
           }
       }
   }
}





