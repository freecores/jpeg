---------------------------------------------------------------------------------------------------
--
-- Title       : JPEG Hardware Compressor Testbench
-- Design      : jpeg
-- Author      : Victor Lopez Lorenzo
-- E-mail      : victor.lopez ((at)) ono ((dot)) com
--
-- License     : GPLv3 (only for NON-COMMERCIAL purposes)
--
---------------------------------------------------------------------------------------------------
--
--
--    Copyright (C) 2004  Victor Lopez Lorenzo
--
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
--
---------------------------------------------------------------------------------------------------
-- 
--    It is the compressor.vhd itself the one who writes the image.jpg output file
--    when simulated with this testbench.
--
---------------------------------------------------------------------------------------------------

library ieee,xilinxcorelib,unisim;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;


entity compressor_tb is
end compressor_tb;

architecture TB_ARCHITECTURE of compressor_tb is
	component compressor
	port(
		clk : in std_logic;
		reset : in std_logic;
      CompressImage : in std_logic; --must be active high for just one cycle
      Compression : in std_logic_vector(1 downto 0); --Quality: 00 = low, 01 = medium, 10 = high
      Mono : in std_logic; --active high for grey-scale input image (Red=Green=Blue)
      ImgColumns : in std_logic_vector(9 downto 0); --columns in each line of the image to compress
      ImgLines : in std_logic_vector(8 downto 0); --lines of the image to compress
      Compressing : out std_logic;
      ProcessRGB : in std_logic;
      ProcessingRGB : out std_logic;
      Red : in std_logic_vector(7 downto 0);
      Green : in std_logic_vector(7 downto 0);
      Blue : in std_logic_vector(7 downto 0);
      addr: out std_logic_VECTOR(15 downto 0);
      din: out std_logic_VECTOR(7 downto 0);
      we: out std_logic);
	end component;

	signal clk : std_logic;
	signal reset : std_logic;
	signal CompressImage : std_logic;
	signal Compression : std_logic_vector(1 downto 0);
   signal Mono : std_logic;
	signal ImgColumns : std_logic_vector(9 downto 0);
	signal ImgLines : std_logic_vector(8 downto 0);
	signal ProcessRGB : std_logic;
	signal Red : std_logic_vector(7 downto 0);
	signal Green : std_logic_vector(7 downto 0);
	signal Blue : std_logic_vector(7 downto 0);

	signal Compressing : std_logic;
	signal ProcessingRGB : std_logic;
	signal addr : std_logic_vector(15 downto 0);
	signal din : std_logic_vector(7 downto 0);
	signal we : std_logic;


   type ByteT is (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,c41,c42,c43,c44,c45,c46,c47,c48,c49,c50,c51,c52,c53,c54,c55,c56,c57,c58,c59,c60,c61,c62,c63,c64,c65,c66,c67,c68,c69,c70,c71,c72,c73,c74,c75,c76,c77,c78,c79,c80,c81,c82,c83,c84,c85,c86,c87,c88,c89,c90,c91,c92,c93,c94,c95,c96,c97,c98,c99,c100,c101,c102,c103,c104,c105,c106,c107,c108,c109,c110,c111,c112,c113,c114,c115,c116,c117,c118,c119,c120,c121,c122,c123,c124,c125,c126,c127,c128,c129,c130,c131,c132,c133,c134,c135,c136,c137,c138,c139,c140,c141,c142,c143,c144,c145,c146,c147,c148,c149,c150,c151,c152,c153,c154,c155,c156,c157,c158,c159,c160,c161,c162,c163,c164,c165,c166,c167,c168,c169,c170,c171,c172,c173,c174,c175,c176,c177,c178,c179,c180,c181,c182,c183,c184,c185,c186,c187,c188,c189,c190,c191,c192,c193,c194,c195,c196,c197,c198,c199,c200,c201,c202,c203,c204,c205,c206,c207,c208,c209,c210,c211,c212,c213,c214,c215,c216,c217,c218,c219,c220,c221,c222,c223,c224,c225,c226,c227,c228,c229,c230,c231,c232,c233,c234,c235,c236,c237,c238,c239,c240,c241,c242,c243,c244,c245,c246,c247,c248,c249,c250,c251,c252,c253,c254,c255);
   subtype Byte is ByteT;
   type ByteFileType is file of Byte;
   file infile : ByteFileType open read_mode is "image.bmp";

   
   -- integer to bit_vector conversion
   function int2bit_vec(A: integer; SIZE: integer) return BIT_VECTOR is
   	variable RESULT: BIT_VECTOR(SIZE-1 downto 0);
   	variable TMP: integer;
   begin
   	TMP:=A;
   	for i in 0 to SIZE-1 loop
   		if TMP mod 2 = 1 then RESULT(i):='1';
   		else RESULT(i):='0';
   		end if;
   		TMP:=TMP / 2;
   	end loop;
   	return RESULT;
   end;

   
begin

	UUT : compressor
		port map (
			clk => clk,
			reset => reset,
			CompressImage => CompressImage,
			Compressing => Compressing,
			Compression => Compression,
         Mono => Mono,
			ImgColumns => ImgColumns,
			ImgLines => ImgLines,
			ProcessRGB => ProcessRGB,
			ProcessingRGB => ProcessingRGB,
			Red => Red,
			Green => Green,
			Blue => Blue,
			addr => addr,
			din => din,
			we => we
		);

   Clocket : process --40 MHz -> T = 25 ns 
   begin
      clk <= '1';
	   wait for 12.5 ns;
      clk <= '0';
	   wait for 12.5 ns;
   end process;

   reset <= '1', '0' after 20 ns;
   
   CompressImage <= '1'   , '0' after 45 ns;

   Compression <= "10"; --"01"; --Medium Quality (Q ITU standard)
   Mono <= '0';
   
   Data : process(clk)
      variable Clock_Up : std_logic := '1';
      variable Prev_Proc : std_logic;
      variable Bloque : std_logic_vector(511 downto 0) := X"30353A3E40404040353C3E41444141413B4045484341414144464745454444444445464747404040464646464542424247474648474242424747464648434343";
      --in decimal it is 48535862646464645360626568656565596469726765656568707169696868686869707171646464707070706966666671717072716666667171707072676767

      variable Line : std_logic_vector(8 downto 0);
      variable Column : std_logic_vector(9 downto 0);
      variable PixelIni : integer range 0 to 511;
      variable Pixel : Byte;
      variable Temp : std_logic_vector(7 downto 0);
      variable JumpHeader : std_logic := '0';      
      variable FinImg : std_logic;
   begin  
      if reset = '1' then
         Red <= (others => '1');
         Green <= (others => '1');
         Blue <= (others => '1');
         Prev_Proc := '1';
         Pixelini := 0;
         ProcessRGB <= '0';
         FinImg := '0';
         if JumpHeader = '0' then
            for i in 0 to 53 loop
               read(infile, Pixel);
               case i is
                  when 18 => --1st byte of Width
                     Column(7 downto 0) := To_Stdlogicvector(int2bit_vec(ByteT'pos(Pixel),8));
                  when 19 => --2nd byte of Width                                  
                     Temp := To_Stdlogicvector(int2bit_vec(ByteT'pos(Pixel),8));
                     Column(9 downto 8) := Temp(1 downto 0);
                  when 22 => --1st byte of Height
                     Line(7 downto 0) := To_Stdlogicvector(int2bit_vec(ByteT'pos(Pixel),8));
                  when 23 => --2nd byte of Height                                                 
                     Temp := To_Stdlogicvector(int2bit_vec(ByteT'pos(Pixel),8));
                     Line(8) := Temp(0);
                  when 24 => --we write to the Compressor the image dimensions
                     ImgColumns <= Column - 1; --remember for a 352x288 it is (0..351)x(0..287)
                     ImgLines <= Line - 1;
                  when others =>
                     null;
               end case;      
            end loop;            
            JumpHeader := '1';
         end if;   
         Line := (others => '0');
         Column := (others => '0');
      elsif (clk = '1' and clk'event) then
         if Prev_Proc = '1' and ProcessingRGB = '0' then
            --PixelIni := 511 - conv_integer(Line(2 downto 0))*64 - conv_integer(Column(2 downto 0))*8;
            --511-504 503-496 495-488 487-480 479-472 471-464 463-456 455-448
            read(infile,Pixel);  
            Blue <= To_Stdlogicvector(int2bit_vec(ByteT'pos(Pixel),8));
            read(infile,Pixel);  
            Green <= To_Stdlogicvector(int2bit_vec(ByteT'pos(Pixel),8));
            read(infile,Pixel);  
            Red <= To_Stdlogicvector(int2bit_vec(ByteT'pos(Pixel),8));
            --Red <= Bloque(PixelIni downto PixelIni - 7);
            --Green <= Bloque(PixelIni downto PixelIni - 7);
            --Blue <= Bloque(PixelIni downto PixelIni - 7);
            ProcessRGB <= '1';
            if Column = ImgColumns then
               Column := (others => '0');
               if Line = ImgLines then
                  Line := (others => '0');
               else
                  Line := Line + 1;
               end if;   
            else   
               Column := Column + 1;
            end if;   
               
         elsif ProcessingRGB = '1' then
            ProcessRGB <= '0';
         end if;

         if FinImg = '0' then
            Prev_Proc := ProcessingRGB;
         else
            Prev_Proc := '0'; --so that it doesn't send again the image
            ProcessRGB <= '0'; --must be done here because in the last cycle, with Line=0 and Column=0 the last pixel is being sent!
            File_Close(infile);
         end if;   

               assert not (FinImg='1' and Compressing = '0')
                  report "Compression Completed"
                     severity FAILURE; --everything went fine, it's just to stop the simulation
                              
        
         if Line = 0 and Column = 0 then
            FinImg := '1';
         end if;   
         
      end if;
   end process Data;      
   
end TB_ARCHITECTURE;

configuration TESTBENCH_FOR_compressor of compressor_tb is
	for TB_ARCHITECTURE
		for UUT : compressor
			use entity work.compressor(jpg);
		end for;
	end for;
end TESTBENCH_FOR_compressor;

