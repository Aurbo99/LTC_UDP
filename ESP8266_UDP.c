#include <WiFi.h>
#include <WiFiUdp.h>

const char* ssid = "YOUR_SSID";
const char* password = "YOUR_PASSWORD";
const IPAddress multicastIP(239, 0, 0, 10); // A common multicast IP range
const unsigned int multicastPort = 12345;
WiFiUDP Udp;

//setup function
void setup() {
  Serial.begin(115200);
  Serial.println();
  Serial.printf("Connecting to %s ", ssid);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("connected");

  // Join the multicast group
  Udp.beginMulticast(WiFi.localIP(), multicastIP, multicastPort);
  Serial.printf("Now listening at IP %s and %s, UDP port %d\n", 
                WiFi.localIP().toString().c_str(), 
                multicastIP.toString().c_str(), 
                multicastPort);
}

//loop function
void loop() {
  // Sending Multicast Packet
  static unsigned long lastSendTime = 0;
  if (millis() - lastSendTime > 5000) { // Send every 5 seconds
    Udp.beginPacketMulticast(multicastIP, multicastPort, WiFi.localIP());
    Udp.printf("Hello from ESP8266! (Timestamp: %lu)", millis());
    Udp.endPacket();
    Serial.println("Sent multicast packet.");
    lastSendTime = millis();
  }

  // Receiving Multicast Packet
  int packetSize = Udp.parsePacket();
  if (packetSize) {
    char packetBuffer[255];
    int len = Udp.read(packetBuffer, sizeof(packetBuffer) - 1);
    packetBuffer[len] = 0; // Null-terminate the string

    Serial.printf("Received packet from %s:%d\n", 
                  Udp.remoteIP().toString().c_str(), 
                  Udp.remotePort());
    Serial.printf("Packet contents: %s\n", packetBuffer);
  }
}

Explanation:

    Udp.beginMulticast(WiFi.localIP(), multicastIP, multicastPort);:
    This line is crucial for the ESP8266 to join the specified multicast group, enabling it to receive packets sent to that group.
    Udp.beginPacketMulticast(multicastIP, multicastPort, WiFi.localIP());:
    When sending, this function prepares the UDP packet for multicast, specifying the destination multicast IP, port, and the local IP address of the ESP8266.
    Udp.parsePacket() and Udp.read():
    These functions are used to check for incoming UDP packets and read their content.
    Multicast IP Address:
    Multicast IP addresses typically fall within the range 224.0.0.0 to 239.255.255.255.

This example provides a basic framework for UDP multicast communication on the ESP8266. You can expand upon it for more complex applications requiring device discovery, data broadcasting, or other multicast-enabled functionalities.

