int A = 8; //1nd bit
int B = 9; //2nd bit
int C = 10; //3rd bit
int D = 11; //4th bit
int W1 = 2;
int W2 = 3;
int W3 = 4;
int W4 = 5;
int Avoltage,one,ten,hundred,thousand,b;
float voltage;
int analog = 5;

void setup() {
  
  Serial.begin(9600);
  pinMode(A,OUTPUT);
  pinMode(B,OUTPUT);
  pinMode(C,OUTPUT);
  pinMode(D,OUTPUT);
  pinMode(W1,OUTPUT);
  pinMode(W2,OUTPUT);
  pinMode(W3,OUTPUT);
  pinMode(W4,OUTPUT);
  digitalWrite(W1,LOW);
  digitalWrite(W2,HIGH);
  digitalWrite(W3,HIGH);
  digitalWrite(W4,HIGH);
  Avoltage=0;
 
}

void loop() {
  // put your main code here, to run repeatedly:
  
  Avoltage = analogRead(5);
  voltage = (((float)Avoltage)/991.0) * 5;
//  Serial.println(voltage);
  printFloat(voltage,4);
  
  one=(int)(voltage*1000)%10;
  ten=((int)(voltage*100)%10);
  hundred=((int)(voltage*10)%10);
  thousand=((int)voltage)%10;
  
  digitalWrite(W1,LOW);
  
  for (b=0;b<4;b++)
    {
     if ( one & (1 << b))
       digitalWrite(b+8,HIGH);
     else
       digitalWrite(b+8,LOW);
    }
  
  digitalWrite(W1,HIGH);
  digitalWrite(W2,LOW);
  
  for (b=0;b<4;b++)
    {
     if ( ten & (1 << b))
       digitalWrite(b+8,HIGH);
     else
       digitalWrite(b+8,LOW);
    }
  
  digitalWrite(W2,HIGH);
  digitalWrite(W3,LOW);
  
  for (b=0;b<4;b++)
    {
     if ( hundred & (1 << b))
       digitalWrite(b+8,HIGH);
     else
       digitalWrite(b+8,LOW);
    }
    
  digitalWrite(W3,HIGH);
  digitalWrite(W4,LOW);
  
   for (b=0;b<4;b++)
    {
     if ( thousand & (1 << b))
       digitalWrite(b+8,HIGH);
     else
       digitalWrite(b+8,LOW);
    }
  
  digitalWrite(W4,HIGH);
  

  //digitalWrite(A,HIGH);
  //digitalWrite(B,HIGH);
  //digitalWrite(C,HIGH);
  //digitalWrite(D,HIGH);
}

// printFloat prints out the float 'value' rounded to 'places' places after the decimal point
void printFloat(float value, int places) {
  // this is used to cast digits
  int digit;
  float tens = 0.1;
  int tenscount = 0;
  int i;
  float tempfloat = value;

    // make sure we round properly. this could use pow from <math.h>, but doesn't seem worth the import
  // if this rounding step isn't here, the value  54.321 prints as 54.3209

  // calculate rounding term d:   0.5/pow(10,places)  
  float d = 0.5;
  if (value < 0)
    d *= -1.0;
  // divide by ten for each decimal place
  for (i = 0; i < places; i++)
    d/= 10.0;    
  // this small addition, combined with truncation will round our values properly
  tempfloat +=  d;

  // first get value tens to be the large power of ten less than value
  // tenscount isn't necessary but it would be useful if you wanted to know after this how many chars the number will take

  if (value < 0)
    tempfloat *= -1.0;
  while ((tens * 10.0) <= tempfloat) {
    tens *= 10.0;
    tenscount += 1;
  }


  // write out the negative if needed
  if (value < 0)
    Serial.print('-');

  if (tenscount == 0)
    Serial.print(0, DEC);

  for (i=0; i< tenscount; i++) {
    digit = (int) (tempfloat/tens);
    Serial.print(digit, DEC);
    tempfloat = tempfloat - ((float)digit * tens);
    tens /= 10.0;
  }

  // if no places after decimal, stop now and return
  if (places <= 0)
    return;

  // otherwise, write the point and continue on
  Serial.print('.');  

  // now write out each decimal place by shifting digits one by one into the ones place and writing the truncated value
  for (i = 0; i < places; i++) {
    tempfloat *= 10.0;
    digit = (int) tempfloat;
    Serial.print(digit,DEC);  
    // once written, subtract off that digit
    tempfloat = tempfloat - (float) digit;
  }
  
  Serial.println();
}






