// Example 4.4
// Color wheel on RGB LED

int p_red = 6;  
int p_grn = 9;
int p_blu = 10;

int pitchOut;

// the setup routine runs once when you press reset:
void setup()  { 
  // declare LEDs to be output
  pinMode(p_red, OUTPUT);
  pinMode(p_grn, OUTPUT);
  pinMode(p_blu, OUTPUT);
} 



void loop() {
  
  
  
  static double hue = 180.0;
  //The Hue value will vary from 0 to 360, which represents degrees in the color wheel

  setLedColorHSV(hue,1,1); //We are using Saturation and Value constant at 1
  hue += 1;
  if( hue >= 360)
    hue = 0;
  delay(10); //each color will be shown for 10 milliseconds
  
  setAudio();
  tone(5,pitchOut);
}

void setAudio()
{
  int sensorReading = analogRead(A0);
  
  pitchOut = map(sensorReading, 10, 800, 120, 1500);
}

//Convert a given HSV (Hue Saturation Value) to RGB(Red Green Blue) and set the led to the color
//  h is hue value, integer between 0 and 360
//  s is saturation value, double between 0 and 1
//  v is value, double between 0 and 1
//http://splinter.com.au/blog/?p=29
void setLedColorHSV(int h, double s, double v) {
  //this is the algorithm to convert from RGB to HSV
  double r=0; 
  double g=0; 
  double b=0;

  double hf=h/60.0;

  int i=(int)floor(h/60.0);
  double f = h/60.0 - i;
  double pv = v * (1 - s);
  double qv = v * (1 - s*f);
  double tv = v * (1 - s * (1 - f));

  switch (i)
  {
  case 0: // red dominant
    r = v;
    g = tv;
    b = pv;
    break;
  case 1: // green
    r = qv;
    g = v;
    b = pv;
    break;
  case 2: 
    r = pv;
    g = v;
    b = tv;
    break;
  case 3: // blue
    r = pv;
    g = qv;
    b = v;
    break;
  case 4:
    r = tv;
    g = pv;
    b = v;
    break;
  case 5: // red
    r = v;
    g = pv;
    b = qv;
    break;
  }

  //set each component to a integer value between 0 and 255
  int red=constrain((int)255*r,0,255);
  int green=constrain((int)255*g,0,255);
  int blue=constrain((int)255*b,0,255);

  setLedColor(red,green,blue);
}

//Sets the current color for the RGB LED
void setLedColor(int red, int green, int blue) {
  //Note that we are reducing 1/4 the intensity for the green and blue components because 
  //  the red one is too dim on my LED. You may want to adjust that.
  analogWrite(p_red,red/5); //Red pin attached to 9
  analogWrite(p_grn,green/6); //Red pin attached to 9
  analogWrite(p_blu,blue/3); //Red pin attached to 9
}


