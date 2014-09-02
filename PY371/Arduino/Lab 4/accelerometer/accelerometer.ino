void setup(){
    Serial.begin(9600);
}

void loop()
{
  int x = analogRead(A0);
  int y = analogRead(A1);
 // int z = analogRead(A0); 
  Serial.print(x);
  Serial.print("    ");
  Serial.println(y);
 /* Serial.print(" ");
  Serial.print(y);
  Serial.print(" ");
  Serial.println(z);*/
  delay(1000);
}
