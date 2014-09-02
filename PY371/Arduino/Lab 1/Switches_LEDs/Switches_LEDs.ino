int s1 = 9;
int s2 = 8;
int s3 = 7;
int s4 = 6;
int led1 = 13;
int led2 = 12;
int led3 = 11;
int led4 = 10;
int val1 = 0;
int val2 = 0;
int val3 = 0;
int val4 = 0;

void setup() {
  // put your setup code here, to run once:
  //Serial.begin(9600);
  pinMode(s1,INPUT);
  pinMode(s2,INPUT);
  pinMode(s3,INPUT);
  pinMode(s4,INPUT);
  digitalWrite(s1,HIGH);
  digitalWrite(s2,HIGH);
  digitalWrite(s3,HIGH);
  digitalWrite(s4,HIGH);
  pinMode(led1,OUTPUT);
  pinMode(led2,OUTPUT);
  pinMode(led3,OUTPUT);
  pinMode(led4,OUTPUT);

}

void loop() {
  // put your main code here, to run repeatedly:
   
   val1 = digitalRead(s1);
   val2 = digitalRead(s2);
   val3 = digitalRead(s3);
   val4 = digitalRead(s4);
//   Serial.println(val4);
   digitalWrite(led1,!val1);
   digitalWrite(led2,!val2);
   digitalWrite(led3,!val3);
   digitalWrite(led4,!val4);
}
