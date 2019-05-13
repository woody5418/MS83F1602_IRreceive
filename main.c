/****************************************************************************
*       Copyright (C), 北京四维拓智教育科技有限公司
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
*后一级输出PC3
*前一级输入PC1(AIN5)
*按键PA3
*拨码PA2
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
#define  Input			 PC0  //前一级输入
/*  Variate ////////////////////////////////////////////////////////////////*/
uint8_t  VS1838_Status=0;           //红外接收处理状态
uint8_t  VS1838_Receive_Count=0;    //红外接收数据位计数
uint8_t  VS1838_receive_ok=0;       //红外接收完成标志
uint32_t VS838_Receive_Data=0;      //32位数据
uint16_t Cycle_CCPR=0;

uint8_t  VS1838_Data=0;      //红外按键的值
uint8_t  StudyVS1838DATA=0;   //学习的红外键值

uint16_t ADCIN4 = 0;       //获取ADC的值
uint8_t  flag_led_put=0;   //LED输出正反转控制
uint8_t  vs_times=0;       //点动模式超时计数


/*  函数声明初始化 --///////////////////////////////////////////////////////*/
void System_init(void);   //系统初始化
void Delay_xms(uint16_t x);  //硬延时初始化
void GPIO_Init(void);     //GPIO管脚初始化
void Timer2_Init(void);
void Timer1_Init(void);
/////void ADC_Init(void);   //adc 初始化

/*  函数声明 --//////////////////////////////////////////////////////////////*/
uint8_t Read_EEPROM_u8(uint8_t EEAddress);   //eeprom 按照u8读取
uint16_t Read_EEPROM_u16(uint8_t EEAddress); //eeprom 按照u16读取
void Write_EEPROM_u8(uint8_t EEAddress,uint8_t EEDatas);   //eeprom 按照u8写入
void Write_EEPROM_u16(uint8_t EEAddress,uint16_t EEDatas); //eeprom 按照u16写入

uint16_t GetADCValue(void);
void LED_Study_End(void);
uint16_t GetCCP1Value(void);
void Clean_countReg(void);
void VS1838ReceiveHandle(uint16_t capdata);//脉宽解码函数
uint8_t VS1838_Process(void);             //数据处理函数
uint8_t isKeyPressed(void);
uint8_t isSwitchOn(void) ;

void Control(void);
/*******************************************************************************
 * 函数名：main
 * 描述  ：系统主函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void main(void)
{
    CLRWDT();//清看门狗
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
        CLRWDT();//清看门狗
        Control();
    }
}
/*******************************************************************************
 * 函数名：System_init
 * 描述  ：系统初始化函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void System_init(void)
{
    OPTION  = 0x00;//BIT7使能上拉位，0=使能，1=禁止
    OSCCON  = 0x70;  //内部振荡器选择 16Mhz
    WDTCON  = 0x1F;  //BIT4~BIT1:看门狗复位时间512ms，bit0--1=开启，0=关闭
}
/*******************************************************************************
 * 函数名：GPIO_Init
 * 描述  ：GPIO初始化函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
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
    CM2=1;//比较器关闭，CxIN管脚为数字IO管脚

    //当弱上拉和弱下拉同时在这些 IO 打开时，弱下拉将被禁止，弱上拉起作用
    //ANSEL portA 1=模拟 0=数字
    ANSEL0=0;
    ANSEL1=0;
    ANSEL2=0;
    ANSEL3=0;
    ANSEL4=0;
    ANSEL5=0;
    ANSEL6=0;
    ANSEL7=0;
    //WPUA 1=使能上拉，0=禁止
    WPUA0=1;
    WPUA1=1;
    WPUA2=1;
    WPUA3=1;
    WPUA4=1;
    WPUA5=1;
    WPUA6=1;
    WPUA7=1;
    //WPUA 1=使能上拉，0=禁止
    WPUC0=1;
    WPUC1=1;
    WPUC2=0;
    WPUC3=0;
    WPUC4=0;
    WPUC5=0;
    //WPD 1=使能下拉，0=断开下拉
    WPDA4=0;
    WPDC1=0;
    WPDC2=0;
    WPDC3=0;
}
/*******************************************************************************
 * 函数名：interrupt ISR
 * 描述  ：中断函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void interrupt ISR(void)
{
    if(CCP1IE&CCP1IF == 1) {
        CCP1IF = 0; //清除中断标志位
        Cycle_CCPR = GetCCP1Value(); //获取捕捉值
        vs_times=0;
        VS1838ReceiveHandle(Cycle_CCPR);
        Clean_countReg();
    }
}
/*******************************************************************************
 * 函数名：Timer2_Init
 * 描述  ：系定时器2初始化函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void Timer2_Init(void)
{
    //T2CON
    TOUTPS0=0;
    TOUTPS1=0;
    TOUTPS2=0;
    TOUTPS3=0;//设置输出后分频比1~16
    T2CKPS0=1;
    T2CKPS1=1;//时钟预分频比00=1 01=4 1x=16

    TMR2=0;//定时器2计数器付初值
    PR2=250;//定时器2周期比较寄存器 PR2的值要大于TMR2  T=(1/16MHz)*16*4*25 = 0.1ms

    //INTCON
    PEIE=1;//外设中断使能 1=enble 0=disable
    TMR2IE=1;//TMR2与PR2比较相等中断使能位 1=enable 0=disable
    TMR2IF=0;//TMR2与PR2比较相等中断标志位 硬件置1，软件清0
    GIE=1; //全局中断使能 1=enble 0=disable
    TMR2ON=1;//打开定时器2使能位
}
/*******************************************************************************
 * 函数名：Timer2_Init
 * 描述  ：系定时器2初始化函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void Timer1_Init(void)
{
    //T1CON
    T1CKPS1 = 1;
    T1CKPS0 = 1;  //预分频比1:8    5us计数一次
    T1OSCEN = 0;//关闭TRM1振荡器
    CCP1CON = 0x05;//CCP1M3 = 0;CCP1M2 = 1;CCP1M1 = 0;CCP1M0 = 0; //捕捉模式，每个下降沿（CCP1IF 位置 1）
    TMR1CS = 0X00;  //内部时钟

    PEIE = 1;   //外设中断使能 1=enble 0=disable
    CCP1IE=1;   //允许CCP1中断
    CCP1IF=0;
    TRISC5 = 1;  //配置为输入模式
    TMR1H=0;
    TMR1L=0;
    GIE=1; //全局中断使能 1=enble 0=disable
    TMR1ON = 1;
}
//获取脉宽值
uint16_t GetCCP1Value(void)
{
    uint16_t values=0;

    values = CCPR1H;
    values = values<<8;
    values = values|CCPR1L;
    return values;
}
//清除计数值 第一个为误值
void Clean_countReg(void)
{
    TMR1H=0;
    TMR1L=0;
}
/*******************************************************************************
 * 函数名：红外接收处理函数
 * 描述  ：系统初始化函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void VS1838ReceiveHandle(uint16_t capdata)
{
    switch(VS1838_Status) {
    case 0: //引导码 9ms + 4.5ms    13.5
        if((capdata >= 3000)&&(capdata <= 7000)) {
            VS1838_Status = 1;
        } else {
            VS1838_Status = 0;
            VS1838_Receive_Count = 0;
        }
        break;
    case 1: //开始32位数据的接收
        if((capdata>=800)&&(capdata<=1500)) { //间隔1.12ms ->0
            VS838_Receive_Data=VS838_Receive_Data<<1;
            VS1838_Receive_Count++;
        } else if((capdata >= 1800)&&(capdata <= 2800)) { //间隔2.25ms ->1
            VS838_Receive_Data=VS838_Receive_Data<<1;
            VS838_Receive_Data=VS838_Receive_Data|0x0001;
            VS1838_Receive_Count++;
        } else {
            VS1838_Status = 0;
            VS838_Receive_Data = 0;
            VS1838_Receive_Count = 0;
        }
        //超出接收数据位数，接收下一个
        while(VS1838_Receive_Count == 32) {
            VS1838_receive_ok = 1;//红外接收完成
            VS1838_Status = 0;
            VS1838_Receive_Count = 0;
            break;
        }
        break;
    default : { //缺省项
        //OUTPUT_L;
        VS1838_Status=0;
    }
    break;
    }
}
//获取返回值
uint8_t VS1838_Process(void)
{
    uint8_t Address_H,Address_L;       //地址码,地址反码
    uint8_t Data_H,Data_L;             //数据码,数据反码

    if(VS1838_receive_ok==1) {        //接收完成
        Address_H=VS838_Receive_Data>>24;                //得到地址码
        Address_L=(VS838_Receive_Data>>16)&0xff;         //得到地址反码
        if((Address_H==(uint8_t)~Address_L)) { //检验遥控识别码(ID)及地址
            Data_H=VS838_Receive_Data>>8;              //得到数据码
            Data_L=VS838_Receive_Data;                 //得到数据反码
            if(Data_H==(uint8_t)~Data_L) {               //接收数据码正确
                VS1838_Data=Data_H;                      //正确键值
                VS1838_Status=0;
                VS838_Receive_Data=0;
                return 1;
            }
        }
    }
    return  0;
}

/*******************************************************************************
 * 函数名：ADC_Init
 * 描述  ：ADC初始化函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void ADC_Init(void)
{
    //配置端口为输入模拟
    TRISC0=1;
    ANSEL4=1;	//配置模拟输入
    //配置ADC模块
    //ADCON1
    DIVS=0;//选择时钟
    ADCS0=0;
    ADCS1=0;
    ADCS2=0;//时钟选择
    //ADCON0
    ADCON0=0B10000001;//右对齐 VDD AN0  使能ADC
    VCFG0=0;//VCFG1=0;//配置参考电压 VCFG1没有定义
    CHS0=0;
    CHS1=0;
    CHS2=1;//模拟通道选择5
    ADFM=1;	//1=右对齐，0=左对齐
    ADON=1;//ADC使能位 1=enable 0=disable

    Delay_xms(1);
    GO_DONE=1;//硬件清0，硬件置1 1=正在转换 0=转换完成
}
//获取10位的ADC值
uint16_t GetADCValue(void)
{
    uint16_t ADC_num=0;

    if(GO_DONE==0) { //ADC是否转换完成
        ADC_num=ADRESH;
        ADC_num=ADC_num<<8;
        ADC_num=ADC_num|ADRESL;
        GO_DONE=1;//软件置1，从新读ADC
    }
    return ADC_num;
}
/*******************************************************************************
 * 函数名：Control
 * 描述  ：流程控制函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void Control(void)
{
    if(isKeyPressed()) { //进入学习模式
        OUTPUT=0; //输出低
        while(!VS1838_Process()) {
            LED_STUDY=1; //输出高
            Delay_xms(1);
            //Delay_xms(100);
            //LED_STUDY=0; //输出低
            //Delay_xms(100);
        }
        Write_EEPROM_u8(EEpromStartAdd,VS1838_Data);
        StudyVS1838DATA = VS1838_Data;
        VS1838_Data=0;
        LED_STUDY=0;
        OUTPUT=0; //输出低
    } else {
        /////ADCIN4 = GetADCValue();  //获取AIN0的ADC值 0-1024
        /////if(ADCIN4 < 512){  //低于2.5V
        if(Input == 0) {
            VS838_Receive_Data=0;
            OUTPUT=0; //输出低
        } else {
            VS1838_Process();
            if(isSwitchOn()) {
                if(VS1838_Data == StudyVS1838DATA) { //开关模式
                    if(flag_led_put == 0) {
                        VS1838_Data=0;
                        flag_led_put=1;
                        OUTPUT=1; //输出高
                    } else {
                        VS1838_Data=0;
                        flag_led_put=0;
                        OUTPUT=0; //输出低
                    }
                }
            } else {
                if(VS1838_Data == StudyVS1838DATA) {
                    OUTPUT=1; //输出高
                    vs_times=0;
                    while(1) {
                        if((vs_times==255)&&(VS1838_Data == StudyVS1838DATA))
                            break;
                        else
                            vs_times++;
                        Delay_xms(1);
                    }
                    VS1838_Data=0;
                    OUTPUT=0; //输出低
                }
            }
        }
    }
}
/*******************************************************************************
 * 函数名：KEY_Scan
 * 描述  ：按键扫描函数
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
uint8_t isKeyPressed(void)  //判断学习按键状态
{
    return  KEY_STUDY ? 0:1;
}

uint8_t isSwitchOn(void)  //判断拨码开关状态
{
    return  DialSwitch ? 0:1;
}
/*******************************************************************************
 * 函数名：LED_Study_End
 * 描述  ：LED效果
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
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
 * 函数名：EEPROM部分
 * 描述  ：包括u8/u16写 、u8/16读取数据 地址从0x00开始
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
//写EEPROM 数据  uint16_t
void Write_EEPROM_u16(uint8_t EEAddress,uint16_t EEDatas)
{
    Write_EEPROM_u8(EEAddress,(EEDatas>>8)&0xff);
    Write_EEPROM_u8(EEAddress+1,EEDatas&0xff);
}
//读EEPROM 数据  uint16_t
uint16_t Read_EEPROM_u16(uint8_t EEAddress)
{
    uint16_t EepromData=0;

    EepromData = Read_EEPROM_u8(EEAddress);
    EepromData = EepromData<<8;
    EepromData |= Read_EEPROM_u8(EEAddress+1);
    return 	EepromData;
}
//写EEPROM 数据  uint8_t
void Write_EEPROM_u8(uint8_t EEAddress,uint8_t EEDatas)
{
    //按照手册的步骤
    GIE=0; //GIE清零
    if(GIE==0&&WR==0) { //判断GIE是否为0，不是继续清零
        EEADR = EEAddress; //写入目标地址
        EEDAT = EEDatas;//写入目标数据
        WREN3=1;
        WREN2=1;
        WREN1=1; //把位WREN3\WREN2\WREN1置1
        WR=1; //把WR置1 ，在写的过程不能改变WREN3\WREN2\WREN1的值，否则编程失败
        GIE=1;
        Delay_xms(2); //eeprom_cnt_2ms  //延时2ms以上
    }
}

//读取EEPROM数据 uint8_t
uint8_t Read_EEPROM_u8(uint8_t EEAddress)
{
    uint8_t EepromData=0;
    EEADR  = EEAddress; //写入目标地址
    RD = 1; //把RD置1，开始读
    NOP();
    EepromData = EEDAT;  //读取存储数据
    return 	EepromData;
}
/*******************************************************************************
 * 函数名：Delay_xms 毫秒级延迟函数
 * 描述  ：最大0xFFFF 65535
 * 输入  ：无
 * 输出  ：无
 * 调用  ：内部调用
 ******************************************************************************/
void Delay_xms(uint16_t x)  //死延时x毫秒
{
    uint16_t i;
    for(i=0; i<x; i++) {
        __delay_ms(1);
        CLRWDT();
    }
}




