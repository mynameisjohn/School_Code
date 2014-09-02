#include <Servo.h>

Servo myservo;
int r=0;
void setup()
{
  myservo.attach(3,800,2400);
}

void loop()
{
  myservo.write(0);
  delay(1000);
  
  
  myservo.write(45);
  delay(1000);
  myservo.write(90);
  delay(1000);
  myservo.write(135);
  delay(1000);
  
  myservo.write(179);
  delay(1000);
}
