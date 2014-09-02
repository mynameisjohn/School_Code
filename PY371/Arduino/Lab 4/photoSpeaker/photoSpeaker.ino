void setup(){}

void loop(){
  
  int sensorReading = analogRead(A0);
  
  int thisPitch = map(sensorReading, 10, 800, 120, 1500);
  tone(3,thisPitch,10);
  delay(1);
}
