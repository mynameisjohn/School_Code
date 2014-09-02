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
int C,count;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
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
  count = 0;
}

void loop() {
  // put your main code here, to run repeatedly:
   
   
    C = digitalRead(s1);
//   Serial.println(val4);

  if(!C){
    count++;
    delay(200);}
  
  Serial.println(count);
  
  for (int b=0;b<=4;b++)
    {
      if (count & (1<<b))
        digitalWrite(b+10,HIGH);
      else
        digitalWrite(b+10,LOW);
    }
    
  if (count>10)
    count=0;
   
    

}
