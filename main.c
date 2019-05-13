/****************************************************************************
*       Copyright (C), ������ά���ǽ����Ƽ����޹�˾
*
*       MS83Fxx02B
*       V0.1
*       2017-12-04
*       Woody  QQ:2490006131
*Description  :      <-MS83F1602AS->
* 				        _________
* 				   VDD|1        16|VSS
*  PA7/OSC1/CLKI/T1CKI|2        15|PA0/AN0/C1IN+/ICSPCLK
*    PA6/OSC2/CLKO/T1G|3    M   14|PA1/AN1/C1IN-/ICSPDAT
* 			 PA5/MCLRB|4    S   13|PA2/AN2/C1OUT/T0CKI
*  	      PC3/P1C/PWM4|5    8   12|PA3/AN3/ATEST1
* 	  PC2/AN6/P1D/PWM5|6    3   11|PC0/AN4/C2IN+/P1F/Vref
* 	  PA4/ATEST0/Vregp|7        10|PC1/AN5/C2IN-/P1E/INT
*   PC5/CCP1/P1A/Vregn|8_________9|PC4/C2OUT/P1B/PWM3
*
*PC5->VS1838
*��һ�����PC3
*ǰһ������PC1(AIN5)
*����PA3
*����PA2
*****************************************************************************/
#include "syscfg.h"
//#include "pic16f685.h"


/*  Defines ////////////////////////////////////////////////////////////////*/
#define  _XTAL_FREQ 		16000000		//16T2
#define  EEpromStartAdd  0x00
#define  uint8_t         unsigned char
#define  uint16_t        unsigned int
#define  uint32_t        unsigned long int

#define  DialSwitch      PA5  //PA2->PA5
#define  KEY_STUDY       PC1  //PA3->PC1
#define  OUTPUT          PC4  //PC3->PC4
#define  LED_STUDY       PC3
#define  Input			 PC0  //ǰһ������
/*  Variate ////////////////////////////////////////////////////////////////*/
uint8_t  VS1838_Status=0;           //������մ���״̬
uint8_t  VS1838_Receive_Count=0;    //�����������λ����
uint8_t  VS1838_receive_ok=0;       //���������ɱ�־
uint32_t VS838_Receive_Data=0;      //32λ����
uint16_t Cycle_CCPR=0;

uint8_t  VS1838_Data=0;      //���ⰴ����ֵ
uint8_t  StudyVS1838DATA=0;   //ѧϰ�ĺ����ֵ

uint16_t ADCIN4 = 0;       //��ȡADC��ֵ
uint8_t  flag_led_put=0;   //LED�������ת����
uint8_t  vs_times=0;       //�㶯ģʽ��ʱ����


/*  ����������ʼ�� --///////////////////////////////////////////////////////*/
void System_init(void);   //ϵͳ��ʼ��
void Delay_xms(uint16_t x);  //Ӳ��ʱ��ʼ��
void GPIO_Init(void);     //GPIO�ܽų�ʼ��
void Timer2_Init(void);
void Timer1_Init(void);
/////void ADC_Init(void);   //adc ��ʼ��

/*  �������� --//////////////////////////////////////////////////////////////*/
uint8_t Read_EEPROM_u8(uint8_t EEAddress);   //eeprom ����u8��ȡ
uint16_t Read_EEPROM_u16(uint8_t EEAddress); //eeprom ����u16��ȡ
void Write_EEPROM_u8(uint8_t EEAddress,uint8_t EEDatas);   //eeprom ����u8д��
void Write_EEPROM_u16(uint8_t EEAddress,uint16_t EEDatas); //eeprom ����u16д��

uint16_t GetADCValue(void);
void LED_Study_End(void);
uint16_t GetCCP1Value(void);
void Clean_countReg(void);
void VS1838ReceiveHandle(uint16_t capdata);//������뺯��
uint8_t VS1838_Process(void);             //���ݴ�����
uint8_t isKeyPressed(void);
uint8_t isSwitchOn(void) ;

void Control(void);
/*******************************************************************************
 * ��������main
 * ����  ��ϵͳ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void main(void)
{
    CLRWDT();//�忴�Ź�
    System_init();
    GPIO_Init();
    Timer1_Init();
    /////ADC_Init();

    OUTPUT=0;
    LED_Study_End();
    StudyVS1838DATA = Read_EEPROM_u8(EEpromStartAdd);
    if(	StudyVS1838DATA == 0XFF)
        StudyVS1838DATA = 0x90;
    while(1) {
        CLRWDT();//�忴�Ź�
        Control();
    }
}
/*******************************************************************************
 * ��������System_init
 * ����  ��ϵͳ��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void System_init(void)
{
    OPTION  = 0x00;//BIT7ʹ������λ��0=ʹ�ܣ�1=��ֹ
    OSCCON  = 0x70;  //�ڲ�����ѡ�� 16Mhz
    WDTCON  = 0x1F;  //BIT4~BIT1:���Ź���λʱ��512ms��bit0--1=������0=�ر�
}
/*******************************************************************************
 * ��������GPIO_Init
 * ����  ��GPIO��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void GPIO_Init(void)
{
    //TRISA 0=OUTPUT,1=INPUT
    TRISA0=0;
    TRISA1=0;
    TRISA2=0;
    TRISA3=0;
    TRISA4=0;
    TRISA5=1;
    TRISA6=0;
    TRISA7=0;
    //TRISC 0=OUTPUT,1=INPUT
    TRISC0=1;
    TRISC1=1;
    TRISC2=0;
    TRISC3=0;
    TRISC4=0;
    TRISC5=0;

    CM0=1;
    CM1=1;
    CM2=1;//�Ƚ����رգ�CxIN�ܽ�Ϊ����IO�ܽ�

    //����������������ͬʱ����Щ IO ��ʱ��������������ֹ��������������
    //ANSEL portA 1=ģ�� 0=����
    ANSEL0=0;
    ANSEL1=0;
    ANSEL2=0;
    ANSEL3=0;
    ANSEL4=0;
    ANSEL5=0;
    ANSEL6=0;
    ANSEL7=0;
    //WPUA 1=ʹ��������0=��ֹ
    WPUA0=1;
    WPUA1=1;
    WPUA2=1;
    WPUA3=1;
    WPUA4=1;
    WPUA5=1;
    WPUA6=1;
    WPUA7=1;
    //WPUA 1=ʹ��������0=��ֹ
    WPUC0=1;
    WPUC1=1;
    WPUC2=0;
    WPUC3=0;
    WPUC4=0;
    WPUC5=0;
    //WPD 1=ʹ��������0=�Ͽ�����
    WPDA4=0;
    WPDC1=0;
    WPDC2=0;
    WPDC3=0;
}
/*******************************************************************************
 * ��������interrupt ISR
 * ����  ���жϺ���
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void interrupt ISR(void)
{
    if(CCP1IE&CCP1IF == 1) {
        CCP1IF = 0; //����жϱ�־λ
        Cycle_CCPR = GetCCP1Value(); //��ȡ��׽ֵ
        vs_times=0;
        VS1838ReceiveHandle(Cycle_CCPR);
        Clean_countReg();
    }
}
/*******************************************************************************
 * ��������Timer2_Init
 * ����  ��ϵ��ʱ��2��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void Timer2_Init(void)
{
    //T2CON
    TOUTPS0=0;
    TOUTPS1=0;
    TOUTPS2=0;
    TOUTPS3=0;//����������Ƶ��1~16
    T2CKPS0=1;
    T2CKPS1=1;//ʱ��Ԥ��Ƶ��00=1 01=4 1x=16

    TMR2=0;//��ʱ��2����������ֵ
    PR2=250;//��ʱ��2���ڱȽϼĴ��� PR2��ֵҪ����TMR2  T=(1/16MHz)*16*4*25 = 0.1ms

    //INTCON
    PEIE=1;//�����ж�ʹ�� 1=enble 0=disable
    TMR2IE=1;//TMR2��PR2�Ƚ�����ж�ʹ��λ 1=enable 0=disable
    TMR2IF=0;//TMR2��PR2�Ƚ�����жϱ�־λ Ӳ����1�������0
    GIE=1; //ȫ���ж�ʹ�� 1=enble 0=disable
    TMR2ON=1;//�򿪶�ʱ��2ʹ��λ
}
/*******************************************************************************
 * ��������Timer2_Init
 * ����  ��ϵ��ʱ��2��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void Timer1_Init(void)
{
    //T1CON
    T1CKPS1 = 1;
    T1CKPS0 = 1;  //Ԥ��Ƶ��1:8    5us����һ��
    T1OSCEN = 0;//�ر�TRM1����
    CCP1CON = 0x05;//CCP1M3 = 0;CCP1M2 = 1;CCP1M1 = 0;CCP1M0 = 0; //��׽ģʽ��ÿ���½��أ�CCP1IF λ�� 1��
    TMR1CS = 0X00;  //�ڲ�ʱ��

    PEIE = 1;   //�����ж�ʹ�� 1=enble 0=disable
    CCP1IE=1;   //����CCP1�ж�
    CCP1IF=0;
    TRISC5 = 1;  //����Ϊ����ģʽ
    TMR1H=0;
    TMR1L=0;
    GIE=1; //ȫ���ж�ʹ�� 1=enble 0=disable
    TMR1ON = 1;
}
//��ȡ����ֵ
uint16_t GetCCP1Value(void)
{
    uint16_t values=0;

    values = CCPR1H;
    values = values<<8;
    values = values|CCPR1L;
    return values;
}
//�������ֵ ��һ��Ϊ��ֵ
void Clean_countReg(void)
{
    TMR1H=0;
    TMR1L=0;
}
/*******************************************************************************
 * ��������������մ�����
 * ����  ��ϵͳ��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void VS1838ReceiveHandle(uint16_t capdata)
{
    switch(VS1838_Status) {
    case 0: //������ 9ms + 4.5ms    13.5
        if((capdata >= 3000)&&(capdata <= 7000)) {
            VS1838_Status = 1;
        } else {
            VS1838_Status = 0;
            VS1838_Receive_Count = 0;
        }
        break;
    case 1: //��ʼ32λ���ݵĽ���
        if((capdata>=800)&&(capdata<=1500)) { //���1.12ms ->0
            VS838_Receive_Data=VS838_Receive_Data<<1;
            VS1838_Receive_Count++;
        } else if((capdata >= 1800)&&(capdata <= 2800)) { //���2.25ms ->1
            VS838_Receive_Data=VS838_Receive_Data<<1;
            VS838_Receive_Data=VS838_Receive_Data|0x0001;
            VS1838_Receive_Count++;
        } else {
            VS1838_Status = 0;
            VS838_Receive_Data = 0;
            VS1838_Receive_Count = 0;
        }
        //������������λ����������һ��
        while(VS1838_Receive_Count == 32) {
            VS1838_receive_ok = 1;//����������
            VS1838_Status = 0;
            VS1838_Receive_Count = 0;
            break;
        }
        break;
    default : { //ȱʡ��
        //OUTPUT_L;
        VS1838_Status=0;
    }
    break;
    }
}
//��ȡ����ֵ
uint8_t VS1838_Process(void)
{
    uint8_t Address_H,Address_L;       //��ַ��,��ַ����
    uint8_t Data_H,Data_L;             //������,���ݷ���

    if(VS1838_receive_ok==1) {        //�������
        Address_H=VS838_Receive_Data>>24;                //�õ���ַ��
        Address_L=(VS838_Receive_Data>>16)&0xff;         //�õ���ַ����
        if((Address_H==(uint8_t)~Address_L)) { //����ң��ʶ����(ID)����ַ
            Data_H=VS838_Receive_Data>>8;              //�õ�������
            Data_L=VS838_Receive_Data;                 //�õ����ݷ���
            if(Data_H==(uint8_t)~Data_L) {               //������������ȷ
                VS1838_Data=Data_H;                      //��ȷ��ֵ
                VS1838_Status=0;
                VS838_Receive_Data=0;
                return 1;
            }
        }
    }
    return  0;
}

/*******************************************************************************
 * ��������ADC_Init
 * ����  ��ADC��ʼ������
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void ADC_Init(void)
{
    //���ö˿�Ϊ����ģ��
    TRISC0=1;
    ANSEL4=1;	//����ģ������
    //����ADCģ��
    //ADCON1
    DIVS=0;//ѡ��ʱ��
    ADCS0=0;
    ADCS1=0;
    ADCS2=0;//ʱ��ѡ��
    //ADCON0
    ADCON0=0B10000001;//�Ҷ��� VDD AN0  ʹ��ADC
    VCFG0=0;//VCFG1=0;//���òο���ѹ VCFG1û�ж���
    CHS0=0;
    CHS1=0;
    CHS2=1;//ģ��ͨ��ѡ��5
    ADFM=1;	//1=�Ҷ��룬0=�����
    ADON=1;//ADCʹ��λ 1=enable 0=disable

    Delay_xms(1);
    GO_DONE=1;//Ӳ����0��Ӳ����1 1=����ת�� 0=ת�����
}
//��ȡ10λ��ADCֵ
uint16_t GetADCValue(void)
{
    uint16_t ADC_num=0;

    if(GO_DONE==0) { //ADC�Ƿ�ת�����
        ADC_num=ADRESH;
        ADC_num=ADC_num<<8;
        ADC_num=ADC_num|ADRESL;
        GO_DONE=1;//�����1�����¶�ADC
    }
    return ADC_num;
}
/*******************************************************************************
 * ��������Control
 * ����  �����̿��ƺ���
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void Control(void)
{
    if(isKeyPressed()) { //����ѧϰģʽ
        OUTPUT=0; //�����
        while(!VS1838_Process()) {
            LED_STUDY=1; //�����
            Delay_xms(1);
            //Delay_xms(100);
            //LED_STUDY=0; //�����
            //Delay_xms(100);
        }
        Write_EEPROM_u8(EEpromStartAdd,VS1838_Data);
        StudyVS1838DATA = VS1838_Data;
        VS1838_Data=0;
        LED_STUDY=0;
        OUTPUT=0; //�����
    } else {
        /////ADCIN4 = GetADCValue();  //��ȡAIN0��ADCֵ 0-1024
        /////if(ADCIN4 < 512){  //����2.5V
        if(Input == 0) {
            VS838_Receive_Data=0;
            OUTPUT=0; //�����
        } else {
            VS1838_Process();
            if(isSwitchOn()) {
                if(VS1838_Data == StudyVS1838DATA) { //����ģʽ
                    if(flag_led_put == 0) {
                        VS1838_Data=0;
                        flag_led_put=1;
                        OUTPUT=1; //�����
                    } else {
                        VS1838_Data=0;
                        flag_led_put=0;
                        OUTPUT=0; //�����
                    }
                }
            } else {
                if(VS1838_Data == StudyVS1838DATA) {
                    OUTPUT=1; //�����
                    vs_times=0;
                    while(1) {
                        if((vs_times==255)&&(VS1838_Data == StudyVS1838DATA))
                            break;
                        else
                            vs_times++;
                        Delay_xms(1);
                    }
                    VS1838_Data=0;
                    OUTPUT=0; //�����
                }
            }
        }
    }
}
/*******************************************************************************
 * ��������KEY_Scan
 * ����  ������ɨ�躯��
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
uint8_t isKeyPressed(void)  //�ж�ѧϰ����״̬
{
    return  KEY_STUDY ? 0:1;
}

uint8_t isSwitchOn(void)  //�жϲ��뿪��״̬
{
    return  DialSwitch ? 0:1;
}
/*******************************************************************************
 * ��������LED_Study_End
 * ����  ��LEDЧ��
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void LED_Study_End(void)
{
    LED_STUDY = 1;
    Delay_xms(100);
    LED_STUDY = 0;
    Delay_xms(100);
    LED_STUDY = 1;
    Delay_xms(100);
    LED_STUDY = 0;
    Delay_xms(100);
    LED_STUDY = 1;
    Delay_xms(100);
    LED_STUDY = 0;
    Delay_xms(100);
}

/*******************************************************************************
 * ��������EEPROM����
 * ����  ������u8/u16д ��u8/16��ȡ���� ��ַ��0x00��ʼ
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
//дEEPROM ����  uint16_t
void Write_EEPROM_u16(uint8_t EEAddress,uint16_t EEDatas)
{
    Write_EEPROM_u8(EEAddress,(EEDatas>>8)&0xff);
    Write_EEPROM_u8(EEAddress+1,EEDatas&0xff);
}
//��EEPROM ����  uint16_t
uint16_t Read_EEPROM_u16(uint8_t EEAddress)
{
    uint16_t EepromData=0;

    EepromData = Read_EEPROM_u8(EEAddress);
    EepromData = EepromData<<8;
    EepromData |= Read_EEPROM_u8(EEAddress+1);
    return 	EepromData;
}
//дEEPROM ����  uint8_t
void Write_EEPROM_u8(uint8_t EEAddress,uint8_t EEDatas)
{
    //�����ֲ�Ĳ���
    GIE=0; //GIE����
    if(GIE==0&&WR==0) { //�ж�GIE�Ƿ�Ϊ0�����Ǽ�������
        EEADR = EEAddress; //д��Ŀ���ַ
        EEDAT = EEDatas;//д��Ŀ������
        WREN3=1;
        WREN2=1;
        WREN1=1; //��λWREN3\WREN2\WREN1��1
        WR=1; //��WR��1 ����д�Ĺ��̲��ܸı�WREN3\WREN2\WREN1��ֵ��������ʧ��
        GIE=1;
        Delay_xms(2); //eeprom_cnt_2ms  //��ʱ2ms����
    }
}

//��ȡEEPROM���� uint8_t
uint8_t Read_EEPROM_u8(uint8_t EEAddress)
{
    uint8_t EepromData=0;
    EEADR  = EEAddress; //д��Ŀ���ַ
    RD = 1; //��RD��1����ʼ��
    NOP();
    EepromData = EEDAT;  //��ȡ�洢����
    return 	EepromData;
}
/*******************************************************************************
 * ��������Delay_xms ���뼶�ӳٺ���
 * ����  �����0xFFFF 65535
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
void Delay_xms(uint16_t x)  //����ʱx����
{
    uint16_t i;
    for(i=0; i<x; i++) {
        __delay_ms(1);
        CLRWDT();
    }
}




