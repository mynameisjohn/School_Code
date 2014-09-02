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
int AND,OR,NOR,XOR;

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

  //holding both one AND two turns off led 1
   AND = (val1&&val2);
   //only way to turn off 2 is bolding both 1 and 3
   OR = (val1||val3);
   //holding both two and 4 turns off led 3
   NOR = !(val2|val4);
   //Led 4 can only be on if EITHER one or three is pushed, not both
   XOR = (val3^val4);

   digitalWrite(led1,AND);
   digitalWrite(led2,OR);
   digitalWrite(led3,XOR);
   digitalWrite(led4,NOR);
}
