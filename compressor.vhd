---------------------------------------------------------------------------------------------------
--
-- Title       : JPEG Hardware Compressor
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
--	Contributors :
--		Peter Eisemann   -  Fixed GetCategory, writes and file declarations in order to
--						simulate code under ModelSim
--                                                                                                 
--
---------------------------------------------------------------------------------------------------
--
--    
--    IMPORTANT NOTES :
--
--    This source code features a compliant JPEG compressor Baseline DCT with
--    Huffman enconding and 2x2 1x1 1x1 subsampling. The header is the widely
--    employed JFIF.
--
--    Baseline DCT JPEG with JFIF header is one of the most used image formats.
--
--    The maximum image size is limited to 352x288
--
--    Another limitation is that the input image must have width and height
--    multiple of 16 (that is, an image 32x32 will produce a strictly compliant
--    JPEG image file, but not a 32x24 or a 24x32 input image, although the resulting
--    image will more likely still be viewable), this is due to the subsampling
--    method employed.
--
--    I apologize if you find this code somewhat messy. When I programmed it I imposed
--    myself very strict deadlines and making it opensource was not in my mind, mainly
--    because, if I were to do it again, I would do it in other way to get much
--    more performance. The main problem faced with this implementation was a very
--	scarce availability of BlockRAM in the target FPGA, so some areas that could
--	perfectly run in parallel, speeding a lot the whole process, had to be made
--	sequential in order to save up BlockRAMs. Anyways, this code works
--	(it functioned as a webcam, attached to a CMOS sensor) and, though not
--	as fast as it could be, it has a good performance.
--    
--    As a part of this project there is a quite useful Testbench that takes as input
--    any BMP (24 bit color) image and compresses it with this code, outputting a JPG
--    file that you can view in your computer.
--
---------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_unsigned.all; --for arithmetic ops
--pragma translate_off
library STD;
use STD.textio.all;
use IEEE.std_logic_textio.all;
library XilinxCoreLib;
--pragma translate_on
library UNISIM; 
use UNISIM.all; 

entity Compressor is port (
 	      clk : in STD_LOGIC;
	      reset : in STD_LOGIC;
         --Control/Status Interface
         CompressImage : in std_logic; --must be active high for just one cycle
         Compression : in std_logic_vector(1 downto 0); --Quality: 00 = low, 01 = medium, 10 = high
         Mono : in std_logic; --active high for grey-scale input image (Red=Green=Blue)
         ImgColumns : in std_logic_vector(9 downto 0); --columns in each line of the image to compress
         ImgLines : in std_logic_vector(8 downto 0); --lines of the image to compress
         Compressing : out std_logic;
         
         --Data Interface
         ProcessRGB : in std_logic;
         ProcessingRGB : out std_logic;
         Red : in std_logic_vector(7 downto 0);
         Green : in std_logic_vector(7 downto 0);
         Blue : in std_logic_vector(7 downto 0);
         
         --JPEG Image BlockRAM (Output) Interface
         addr: out std_logic_VECTOR(15 downto 0);
         din: out std_logic_VECTOR(7 downto 0);
         we: out std_logic);
end Compressor;

architecture JPG of Compressor is

--pragma translate_off

file Debug:   TEXT open WRITE_MODE is "Debug.txt";
file DebugY:  TEXT open WRITE_MODE is "DebugY.txt";
file DebugCb: TEXT open WRITE_MODE is "DebugCb.txt";
file DebugCr: TEXT open WRITE_MODE is "DebugCr.txt";

--   file Debug:TEXT is out "Debug.txt";
--   file DebugY:TEXT is out "DebugY.txt";
--	file DebugCb:TEXT is out "DebugCb.txt";
--   file DebugCr:TEXT is out "DebugCr.txt";
	constant espacio:string:=" ";
   constant espacios:string:="  ";
   constant puntoycoma:string:=";";
	constant strElemento:string:=" Element: ";
   constant strColumna:string:=" Column: ";
   constant strLinea:string:=" Line: ";
--pragma translate_on


   component dct2d port (
   	ND: IN std_logic;
   	RDY: OUT std_logic;
   	RFD: OUT std_logic;
   	CLK: IN std_logic;
   	DIN: IN std_logic_VECTOR(7 downto 0);
   	DOUT: OUT std_logic_VECTOR(18 downto 0));
   end component;

   component buffer_comp port (
   	addr: IN std_logic_VECTOR(12 downto 0);
   	clk: IN std_logic;
   	din: IN std_logic_VECTOR(11 downto 0);
   	dout: OUT std_logic_VECTOR(11 downto 0);
   	we: IN std_logic);
   end component;
   
   component buffer_comp_chrom port (
   	addr: IN std_logic_VECTOR(10 downto 0);
   	clk: IN std_logic;
   	din: IN std_logic_VECTOR(11 downto 0);
   	dout: OUT std_logic_VECTOR(11 downto 0);
   	we: IN std_logic);
   end component;
   
   component q_rom port (
   	addr: IN std_logic_VECTOR(8 downto 0);
   	clk: IN std_logic;
   	dout: OUT std_logic_VECTOR(12 downto 0));
   end component;
   
   component huff_rom port (
	   addr: IN std_logic_VECTOR(8 downto 0);
	   clk: IN std_logic;
	   dout: OUT std_logic_VECTOR(19 downto 0));
   end component;

   component tabla_q
   	port (
   	addr: IN std_logic_VECTOR(8 downto 0);
   	clk: IN std_logic;
   	dout: OUT std_logic_VECTOR(7 downto 0));
   end component;   

   --signals for tabla_q
   signal addrTablaQ: std_logic_VECTOR(8 downto 0);
  	signal doutTablaQ: std_logic_VECTOR(7 downto 0);

   
   --signal for huff_rom
   signal addrH : std_logic_vector(8 downto 0);
   signal doutH : std_logic_vector(19 downto 0);
   
   --signals for DCT block
	signal ND: std_logic;
	signal RDY: std_logic;
	signal RFD: std_logic;
	signal DIND: std_logic_VECTOR(7 downto 0);
	signal DOUTD: std_logic_VECTOR(18 downto 0);
   
   --signals for compression buffer
   signal addrY: std_logic_VECTOR(12 downto 0);
	signal dinY: std_logic_VECTOR(11 downto 0);
	signal doutY: std_logic_VECTOR(11 downto 0);
	signal weY: std_logic;
   signal addrCb: std_logic_VECTOR(10 downto 0);
	signal dinCb: std_logic_VECTOR(11 downto 0);
	signal doutCb: std_logic_VECTOR(11 downto 0);
	signal weCb: std_logic;
   signal addrCr: std_logic_VECTOR(10 downto 0);
	signal dinCr: std_logic_VECTOR(11 downto 0);
	signal doutCr: std_logic_VECTOR(11 downto 0);
	signal weCr: std_logic;                      
   
   signal addrY1: std_logic_VECTOR(12 downto 0);
   signal addrCb1: std_logic_VECTOR(10 downto 0);   
   signal addrCr1: std_logic_VECTOR(10 downto 0);   
   signal addrY2: std_logic_VECTOR(12 downto 0);   
   signal addrCb2: std_logic_VECTOR(10 downto 0);   
   signal addrCr2: std_logic_VECTOR(10 downto 0);   

	signal dinY1: std_logic_VECTOR(11 downto 0);
   signal dinY2: std_logic_VECTOR(11 downto 0);
   signal dinCb1: std_logic_VECTOR(11 downto 0);
   signal dinCb2: std_logic_VECTOR(11 downto 0);
   signal dinCr1: std_logic_VECTOR(11 downto 0);
   signal dinCr2: std_logic_VECTOR(11 downto 0);

   signal weY1: std_logic;
   signal weY2: std_logic;
	signal weCb1: std_logic;
   signal weCb2: std_logic;
	signal weCr1: std_logic;
   signal weCr2: std_logic;
   
   --signals for the quantization coefficients ROM
   signal addrQ : std_logic_vector(8 downto 0);
   signal doutQ : std_logic_vector(12 downto 0);

   
   
   signal addri : std_logic_vector(15 downto 0); --to write directly to the port (headers and JPEG size) and read from it
   signal addribk : std_logic_vector(15 downto 0); --exclusive when signal Save='1', it holds the current pixel
   constant MaxImageSize : std_logic_vector(15 downto 0) :="1100011111111100"; --51196 bytes
   
   signal ColumnToCompress : std_logic_vector(9 downto 0);
   signal LineToCompress : std_logic_vector(3 downto 0); --goes from 0 to 15, the 16 that may occupy the luminance buffer
   signal LineAbsToCompress: std_logic_vector(8 downto 0);

   signal MakeDCT : std_logic;
   signal CompressingInt : std_logic;
   
   signal Done : std_logic; --the Huffman encoding part rises it for one cycle when finishes

   signal StepV : integer range 0 to 5;
   signal Save : std_logic;
   signal NDe : std_logic;                   
   signal WriteAdditionalBits : std_logic;

   
   signal WriteTables : std_logic;
   signal TableData : std_logic_vector(5 downto 0);
   signal Table : std_logic;
   
   signal ZRLing : std_logic;
   signal RFDInt : std_logic;
   signal RFDIntData : std_logic_vector(7 downto 0);   
   
   function Multiplier (Num, Prod : in std_logic_vector) return std_logic_vector;
   
	function Multiplier (Num, Prod : in std_logic_vector) return std_logic_vector is
		variable result : std_logic_vector(19 downto 0) := (others => '0');
	begin --8 bits * 10 bits both unsigned = 18 bits
      result := ('0' & Num(7 downto 0)) * ('0' & Prod);
		return result(17 downto 0);
	end Multiplier;

   function MultiplierQ (Num, Prod : in std_logic_vector) return std_logic_vector;
   
	function MultiplierQ (Num, Prod : in std_logic_vector) return std_logic_vector is
      variable result : std_logic_vector(26 downto 0);
      variable UNum : std_logic_vector(11 downto 0);
	begin --it is like Multiplier but admits bigger operands: Num (10..0) (signed) and Prod (10..0) (unsigned)
      --max result = 1000_0000_0000 * 111_1111_1111 = 1(sign)11_1111_1111_1000_0000_0000 (-2.048 * 2.047 = -4.192.256)
      --UPDATE: now Prod may be of up to 13 bits (12..0), so the result will be 24..0
      if Num(Num'High) = '1' then --negative number?
         UNum := not (Num) + 1; --two's complement to make it positive
      else
         UNum := Num;
      end if;   
      result := ('0' & UNum) * ('0' & Prod);
      if Num(Num'High) = '1' then --negative result
         result := (not result) + 1; --2's Complement
      end if;
      return result(24 downto 0);
	end MultiplierQ;
   
   function Mult_Columns (Line : in std_logic_vector) return std_logic_vector;
   
	function Mult_Columns (Line : in std_logic_vector) return std_logic_vector is
		variable result : std_logic_vector(12 downto 0);
	begin
      result := "0000000000000" + (Line & "00000000") + (Line & "000000") + (Line & "00000");
		return result; --with Line max=1111, the max. result will be=1010010100000 (12..0)
	end Mult_Columns;

   function Mult_Half_Columns (Line : in std_logic_vector) return std_logic_vector;
   
	function Mult_Half_Columns (Line : in std_logic_vector) return std_logic_vector is
		variable result : std_logic_vector(10 downto 0);
	begin
      result := "00000000000" + (Line & "0000000") + (Line & "00000") + (Line & "0000");
		return result; --with Line max=111, the max. result will be=10011010000 (10..0)
	end Mult_Half_Columns;     


   function GetCategory (Coef : in std_logic_vector) return integer;
      --function fixed to work under ModelSim by Peter Eisemann
      function GetCategory (Coef : in std_logic_vector) return integer is
         --tells us the category of the coefficient (AC and DC) based on a "sign-less" version of itself!
         variable Coeff : std_logic_vector(Coef'High downto 0);
         variable result: integer := 0;
      begin
         if Coef(Coef'High) = '1' then
            Coeff := (not Coef) + 1;
         else
            Coeff := Coef;
         end if;   
         categoryloop:for index in Coeff'range loop
            if Coeff(index) = '1' then
               -- return (index + 1);  Eim
               result := (index +1);
               exit categoryloop when Coeff(index) = '1';
            end if;   
         end loop categoryloop;               
         return result;
   end GetCategory;   

   
--   function GetCategory (Coef : in std_logic_vector) return integer;
--   
--   function GetCategory (Coef : in std_logic_vector) return integer is
--      --tells us the category of the coefficient (AC and DC) based on a "sign-less" version of itself!
--      variable Coeff : std_logic_vector(Coef'High downto 0);
--   begin
--      if Coef(Coef'High) = '1' then
--         Coeff := (not Coef) + 1;
--      else
--        Coeff := Coef;
--      end if;   
--      for index in Coeff'range loop
--         if Coeff(index) = '1' then
--            return (index + 1);
--        end if;   
--      end loop;               
--      return 0;
--   end GetCategory;   
   
   function AppendHuffmanWord (HuffmanWord, Code : in std_logic_vector; Pos : in integer) return std_logic_vector;
   
   function AppendHuffmanWord (HuffmanWord, Code : in std_logic_vector; Pos : in integer) return std_logic_vector is
      variable result : std_logic_vector(22 downto 0);   
   begin
      result := HuffmanWord;
      for i in (Code'length-1) downto 0 loop
        result(Pos-i) := Code(i); --Code(Code'length-1-i); --MSB first!!
        --IMPORTANT: the std_logic_vector is "to", not "downto", that's why the MSB is opposite as usual
      end loop;   
      return result;
   end AppendHuffmanWord;

   --this function is an overload with Code as std_logic (used when it must only append the sign)
   function AppendHuffmanWord (HuffmanWord : in std_logic_vector; Code : in std_logic; Pos : in integer) return std_logic_vector;
   
   function AppendHuffmanWord (HuffmanWord : in std_logic_vector; Code : in std_logic; Pos : in integer) return std_logic_vector is
      variable result : std_logic_vector(22 downto 0);
   begin
      result := HuffmanWord;
      result(Pos) := Code;      
      return result;
   end AppendHuffmanWord;
   
   --this one is to define the MSB of Code in case it is not length-1, so that CodeLength is the new length-1
   function AppendHuffmanWordL (HuffmanWord, Code : in std_logic_vector; CodeLength : in integer; Pos : in integer) return std_logic_vector;
   
   function AppendHuffmanWordL (HuffmanWord, Code : in std_logic_vector; CodeLength : in integer; Pos : in integer) return std_logic_vector is
      variable result : std_logic_vector(22 downto 0);
   begin
      result := HuffmanWord;
      for i in Code'length downto 0 loop
         if i < CodeLength then --this may look redundant but it avoids an "unbound loop" error
            result(Pos-i) := Code(CodeLength-1-i); --careful! here bit 0 is the LSB, X-File
         end if;   
      end loop;   
      return result;
   end AppendHuffmanWordL;   
   
   function To_std_logicvpor11(ZeroRun : in integer) return std_logic_vector;
   
   function To_std_logicvpor11(ZeroRun : in integer) return std_logic_vector is
      --returns the integer times 11 in a std_logic_vector(8 downto 0)
   begin
      case ZeroRun is
         when 0 =>
            return "000000000";
         when 1 =>
            return "000001011";
         when 2 =>
            return "000010110";
         when 3 =>
            return "000100001";
         when 4 =>
            return "000101100";
         when 5 =>
            return "000110111";
         when 6 =>
            return "001000010";
         when 7 =>
            return "001001101";
         when 8 =>
            return "001011000";
         when 9 =>
            return "001100011";
         when 10 =>
            return "001101110";
         when 11 =>
            return "001111001";
         when 12 =>
            return "010000100";
         when 13 =>
            return "010001111";
         when 14 =>
            return "010011010";
         when others => --15 =>
            return "010100101"; --165
      end case;
   end To_std_logicvpor11;
      
   function To_std_logicv(Cat : in integer) return std_logic_vector;
   
   function To_std_logicv(Cat : in integer) return std_logic_vector is
   begin
      case Cat is
         when 0 =>
            return "0000";
         when 1 =>
            return "0001";
         when 2 =>
            return "0010";
         when 3 =>
            return "0011";
         when 4 =>
            return "0100";
         when 5 =>
            return "0101";
         when 6 =>
            return "0110";
         when 7 =>
            return "0111";
         when 8 =>
            return "1000";
         when 9 =>
            return "1001";
         when others => -- 10 => there won't be 11 because we only use it for AC
            return "1010";
      end case;         
   end To_std_logicv;
   
   function GetMagnitude (Coef : in std_logic_vector; Cat : in integer) return std_logic_vector;
   
   function GetMagnitude (Coef : in std_logic_vector; Cat : in integer) return std_logic_vector is
   begin
      case Cat is
         when 0 =>
            return "000000000000"; --we avoid this case with an if because it wouldn't be correct
         when 1 =>
            return "000000000000"; --we avoid this case with an if because it wouldn't be correct
         when 2 =>
            return (Coef - "10");
         when 3 =>
            return (Coef - "100");
         when 4 =>
            return (Coef - "1000");
         when 5 =>
            return (Coef - "10000");
         when 6 =>
            return (Coef - "100000");
         when 7 =>
            return (Coef - "1000000");
         when 8 =>
            return (Coef - "10000000");
         when 9 =>
            return (Coef - "100000000");
         when 10 =>
            return (Coef - "1000000000");
         when others => --11 =>
            return (Coef - "10000000000");
      end case;
   end GetMagnitude;   
   
   function CompressDC(Cat : in integer; LumaBlock : in std_logic) return std_logic_vector;
   
   function CompressDC(Cat : in integer; LumaBlock : in std_logic) return std_logic_vector is
      variable result : std_logic_vector(14 downto 0) := (others => '0');
   begin --the four MSBs of result keep the number of the MSB bit of the data in the LSBs
      if LumaBlock = '1' then --compress with DC Luminance Table
         case Cat is
            when 0 =>
               result := "000100000000000";
            when 1 =>
               result := "001000000000010";
            when 2 =>
               result := "001000000000011";
            when 3 =>
               result := "001000000000100";
            when 4 =>
               result := "001000000000101";
            when 5 =>
               result := "001000000000110";
            when 6 =>
               result := "001100000001110";
            when 7 =>
               result := "010000000011110";
            when 8 =>
               result := "010100000111110";
            when 9 =>
               result := "011000001111110";
            when 10 =>
               result := "011100011111110";
            when others => --11
               result := "100000111111110";
         end case;
      else --DC chrominance table
         case Cat is
            when 0 =>
               result := "000100000000000";
            when 1 =>
               result := "000100000000001";
            when 2 =>
               result := "000100000000010";
            when 3 =>
               result := "001000000000110";
            when 4 =>
               result := "001100000001110";
            when 5 =>
               result := "010000000011110";
            when 6 =>
               result := "010100000111110";
            when 7 =>
               result := "011000001111110";
            when 8 =>
               result := "011100011111110";
            when 9 =>
               result := "100000111111110";
            when 10 =>
               result := "100101111111110";
            when others => --11
               result := "101011111111110";
         end case;
      end if;   
      return result;
   end CompressDC;
      
begin
   
   with Save select addr <= addribk when '1', addri when others;
   
   with CompressingInt select addrY <= addrY2 when '1', addrY1 when others;
   with CompressingInt select addrCb <= addrCb2 when '1', addrCb1 when others;
   with CompressingInt select addrCr <= addrCr2 when '1', addrCr1 when others;
   
   with CompressingInt select weY <= weY2 when '1', weY1 when others;
   with CompressingInt select weCb <= weCb2 when '1', weCb1 when others;
   with CompressingInt select weCr <= weCr2 when '1', weCr1 when others;
   
   with CompressingInt select dinY <= dinY2 when '1', dinY1 when others;
   with CompressingInt select dinCb <= dinCb2 when '1', dinCb1 when others;
   with CompressingInt select dinCr <= dinCr2 when '1', dinCr1 when others;
   
   DCT1 : dct2d port map (ND,RDY,RFD,clk,DIND,DOUTD);

   buffer_compY : buffer_comp port map (addrY,clk,dinY,doutY,weY);
   buffer_compCb : buffer_comp_chrom port map (addrCb,clk,dinCb,doutCb,weCb);
   buffer_compCr : buffer_comp_chrom port map (addrCr,clk,dinCr,doutCr,weCr);

   Q_ROM1 : q_rom port map (addrQ,clk,doutQ);
   Huffman_ROM : huff_rom port map (addrH,clk,doutH);
   Tabla_Q1 : tabla_q port map(addrTablaQ,clk,doutTablaQ);

   
   
   
   RGB2YCbCr : process(reset, clk)
      --It applies the transformation from RGB to YCBCr and pass it to the JPEG process
      -- but what we are going to save in the buffers will be the pixels of the components with
      -- the JPEG level shift already applied, so that the transformation's last addition: [0;128;128] will become -[128;0;0]
      variable Baton : integer range 0 to 3 := 0; --indicates the current state of the FSM
      variable Red1 : std_logic_vector(17 downto 0);
      variable Red2 : std_logic_vector(17 downto 0);
      variable Red3 : std_logic_vector(17 downto 0);
      variable Green1 : std_logic_vector(17 downto 0);
      variable Green2 : std_logic_vector(17 downto 0);
      variable Green3 : std_logic_vector(17 downto 0);
      variable Blue1 : std_logic_vector(17 downto 0);
      variable Blue2 : std_logic_vector(17 downto 0);
      variable Blue3 : std_logic_vector(17 downto 0);
      
      variable Cb, Cr : std_logic_vector(10 downto 0);
   begin
      if (reset = '1') then
         Compressing <= '0';
         addrY1 <= (others => '0');
         addrCb1 <= (others => '0');
         addrCr1 <= (others => '0');
         weY1 <= '0';
         weCb1 <= '0';
         weCr1 <= '0';
         dinCb1 <= (others => '0');
         dinCr1 <= (others => '0');
         dinY1 <= (others => '0');
         LineToCompress <= "0000";
         LineAbsToCompress <= (others => '0');
         ColumnToCompress <= (others => '0');
         Baton := 0;
         Cb := (others => '0');
         Cr := (others => '0');
         ProcessingRGB <= '0';
         MakeDCT <= '0';
         Red1 := (others => '0');
         Red2 := (others => '0');
         Red3 := (others => '0');
         Green1 := (others => '0');
         Green2 := (others => '0');
         Green3 := (others => '0');
         Blue1 := (others => '0');
         Blue2 := (others => '0');
         Blue3 := (others => '0');
      elsif (clk = '1' and clk'event) then
         if CompressImage = '1' then
            Compressing <= '1';
            addrY1 <= (others => '0');
            addrCb1 <= (others => '0');
            addrCr1 <= (others => '0');
            weY1 <= '0';
            weCb1 <= '0';
            weCr1 <= '0';
            
            LineToCompress <= "0000";
            LineAbsToCompress <= (others => '0');
            ColumnToCompress <= (others => '0');
            Baton := 0;
            ProcessingRGB <= '0';
         end if;

         
         if (ProcessRGB = '1') then
            ProcessingRGB <= '1'; --while this one is high, there won't be another ProcessRGB='1'
            Baton := 1;
         end if;

         addrY1 <= Mult_Columns(LineToCompress) + ColumnToCompress;
         addrCb1 <= Mult_Half_Columns(LineToCompress(3 downto 1)) + (ColumnToCompress(9 downto 1)); --for the subsampling
         addrCr1 <= Mult_Half_Columns(LineToCompress(3 downto 1)) + (ColumnToCompress(9 downto 1));
         --we pre-read the saved data, so as to average the chroma components Cb and Cr
         --to be able to do a proper subsampling 2x2 1x1 1x1
         
         case Baton is
            when 0 =>
               weY1 <= '0';
               weCb1 <= '0';
               weCr1 <= '0';
               MakeDCT <= '0';

            when 1 =>                                    
               --we apply only the transformation RGB to YCbCr
               Red1 := Multiplier(Red, "0010011001"); --153
               Red2 := Multiplier(Red, "0010101101"); --173
               Red3 := "0000000000" & Red; --1
               Green1 := Multiplier(Green, "1001011001"); --601
               Green2 := Multiplier(Green, "0101010011"); --339
               Green3 := Multiplier(Green, "0110101101"); --429
               Blue1 := Multiplier(Blue, "0001110101"); --117
               Blue2 := "0000000000" & Blue; --1
               Blue3 := Multiplier(Blue, "0001010011"); --83
               --the largest obtainable result would be 255*601=153255 (100101011010100111)
               Baton := 2;
               
               MakeDCT <= '0';
            when 2 =>
               --dinY1 <= "111110000000"; --for debugging, to make Y zero, so that in the resulting image the Blue channel will be Cb and the Red one will be Cr
               dinY1 <= "0000" & (Red1(16 downto 9) + Green1(17 downto 10) + Blue1(17 downto 10) - "10000000");-- + Red1(8) + Green1(9) + Blue1(9)); --Red1/512+Green1/1024+Blue1/1024 and with -128 for the level shift
               if Mono = '1' then
                  Cb := (others => '0');
                  Cr := (others => '0');
               else
                  Cb := "000" & (Blue2(8 downto 1) - Red2(17 downto 10) - Green2(17 downto 10)); -- + Blue2(0) - Red2(9) - Green2(9)); --Red & Green between 1024 and Blue between 2
                  Cr := "000" & (Red3(8 downto 1) - Green3(17 downto 10) - Blue3(17 downto 10)); -- + Red3(0) - Green3(9) - Blue3(9)); --between 1024 all but Red
                  --debug: the bits added/substracted at the end are the nearest integer rounding
               end if;   
               
               --Subsampling: average groups of 4 pixels in blocks of 2x2
               if LineAbsToCompress(0) = '0' and ColumnToCompress(0) = '0' then --element (0,0)
                  --dinCb1 <= "0000" & Cb(7 downto 0);
                  --dinCr1 <= "0000" & Cr(7 downto 0);
                  dinCb1 <= "0000" & Cb(7 downto 0);
                  dinCr1 <= "0000" & Cr(7 downto 0);
               elsif LineAbsToCompress(0) = '0' and ColumnToCompress(0) = '1' then --element (0,1)
                  Cb := (Cb(7) & Cb(7) & Cb(7) & Cb(7 downto 0)) + (doutCb(7) & doutCb(7 downto 0));
                  Cr := (Cr(7) & Cr(7) & Cr(7) & Cr(7 downto 0)) + (doutCr(7) & doutCr(7 downto 0));
                  dinCb1 <= "000" & Cb(8 downto 0);
                  dinCr1 <= "000" & Cr(8 downto 0);   
               elsif LineAbsToCompress(0) = '1' and ColumnToCompress(0) = '0' then --element (1,0)
                  Cb := (Cb(7) & Cb(7) & Cb(7) & Cb(7 downto 0)) + (doutCb(8) & doutCb(8 downto 0));
                  Cr := (Cr(7) & Cr(7) & Cr(7) & Cr(7 downto 0)) + (doutCr(8) & doutCr(8 downto 0));
                  dinCb1 <= "00" & Cb(9 downto 0);
                  dinCr1 <= "00" & Cr(9 downto 0);   
               else --element (1,1) before, we have added directly, now we add and write the average of the 4 pixels, so as to not lose precission
                  Cb := (Cb(7) & Cb(7) & Cb(7) & Cb(7 downto 0)) + (doutCb(9) & doutCb(9 downto 0));
                  Cr := (Cr(7) & Cr(7) & Cr(7) & Cr(7 downto 0)) + (doutCr(9) & doutCr(9 downto 0));
                  dinCb1 <= "0000" & Cb(9 downto 2);
                  dinCr1 <= "0000" & Cr(9 downto 2);

                  --next lines are for debugging purposes (instead of the last if,elsifs and else to write just
                  --one chrominance value for Cb and Cr instead of averaging)
                  --Cb := (Cb(7) & Cb(7) & Cb(7) & Cb(7 downto 2)) + (doutCb(7) & doutCb(7 downto 0));
                  --Cr := (Cr(7) & Cr(7) & Cr(7) & Cr(7 downto 2)) + (doutCr(7) & doutCr(7 downto 0));
                  --dinCb1 <= "0000" & Cb(7 downto 0);
                  --dinCr1 <= "0000" & Cr(7 downto 0);
                  --weCb1 <= '1';
                  --weCr1 <= '1';               
               --else
                  --weCb1 <= '0';
                  --weCr1 <= '0';               
               end if;   
                  
               weY1 <= '1';
               weCb1 <= '1';
               weCr1 <= '1';

               if (LineToCompress(2 downto 0) = "111") then
                  if (ColumnToCompress(2 downto 0) = "111") then --we've just written pixel 64 of the block [of 64 pixels]
                     MakeDCT <= '1';                                                             
                  else
                     MakeDCT <= '0';
                  end if;   
               else                  
                  MakeDCT <= '0';
               end if;
               
               if (ColumnToCompress = ImgColumns) then
                  LineToCompress <= LineToCompress + 1;
                  LineAbsToCompress <= LineAbsToCompress + 1;
                  ColumnToCompress <= (others => '0');
               else
                  ColumnToCompress <= ColumnToCompress + 1;
               end if;               
               
               Baton := 3;
            when 3 => --with this dummy cycle we give time to CompressingInt to rise, if it must, and make the following "if" work fine
               weY1 <= '0';
               weCb1 <= '0';
               weCr1 <= '0';
               MakeDCT <= '0';
               Baton := 0;
         end case;
         --MakeDCT is rised in cycle 0, in cycle 1 it is read by the process JPEG which then rises CompressingInt
         --and in cycle 2 this process reads CompressingInt='1' so that in that intermediate cycle 1 is when
         --the case "when 3 =>" gets executed and the following "if", in which MakeDCT is 1 and CompressingInt is 0
         --but it is going to be inverted.
         if (Baton = 0 and MakeDCT = '0' and CompressingInt = '0') then --it is fine!
            if (LineAbsToCompress > ImgLines) then
               Compressing <= '0'; --Compression has ended, image ready
               LineAbsToCompress <= (others => '0'); --absolutely incredible, without this line it will only work for the
                              --first image (it took me much time to debug this one, folks)
            end if;   
            ProcessingRGB <= '0';
         end if;   
         
      end if;   
   end process RGB2YCbCr;   

   
   
   
   
   
   JPEG : process (reset, clk)
      --in this process data are sent to the DCT1 component, its output is quantized and written back
      --in the same addresses where they were read (buffers) to be sent to DCT1.
      variable Columna : std_logic_vector(9 downto 0);
      variable Linea : std_logic_vector(3 downto 0); --0 to 15
      variable Bloque : std_logic_vector(1 downto 0);
      variable DCTQ : std_logic_vector(24 downto 0); --for multiplication of the result of the DCT with numerator of Q
      
      variable Base : std_logic_vector(8 downto 0); --to access Q coefficients from the tables in ROM
      variable BaseH : std_logic_vector(8 downto 0); --for the Huffman tables ROM
      variable BaseQ : std_logic_vector(8 downto 0); --for the Q tables ROM
     
      variable HuffmanWord : std_logic_vector(22 downto 0);
      variable HuffmanWordPos : integer range -1 to 22;
      variable LumaBlock : std_logic; 
      variable Elemento : std_logic_vector(1 downto 0);
      variable Coeficiente : integer range 0 to 63; --indicates which is the next coefficient to be processed by Huffman
      variable PrevDC : std_logic_vector(11 downto 0); --previous value of luminance DC
      variable Coef : std_logic_vector(11 downto 0); --current coefficient's value
      variable LastBlockDCY : std_logic_vector(11 downto 0);
      variable LastBlockDCCb : std_logic_vector(11 downto 0);
      variable LastBlockDCCr : std_logic_vector(11 downto 0);
      variable IniDC : std_logic;
      variable FirstDC : std_logic;
      variable GetPrevDC : std_logic;
      variable ColBk : std_logic_vector(9 downto 0);
      variable LinBk : std_logic_vector(3 downto 0); --0 to 15
      variable Hlength : integer range 0 to 15;
      variable ZeroRun : integer range 0 to 16;
      variable ZRL : integer range 0 to 3;
      variable WriteZRL : std_logic;
      variable Cat : integer range 0 to 11;
      variable Sign : std_logic;
      variable Primera : std_logic;
      variable HeaderFinal : std_logic; --to know if we've already written EOI (End Of Image)
      variable VarTamImg : std_logic_vector(10 downto 0);
      variable DatoHeader : integer range 0 to 7 := 0; --to know where we are in the writing of the header's image size
      variable TempCompDC : std_logic_vector(14 downto 0);
      variable AddVal : std_logic_vector(11 downto 0);
      variable QDC : std_logic_vector(12 downto 0);
      --pragma translate_off
      
      --I couldn't find any testbench where I could read or write binary files and the ones I found were overcomplex
      --so I experimented and found this to be a useful way to do it without any complications.
      type ByteT is (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,c41,c42,c43,c44,c45,c46,c47,c48,c49,c50,c51,c52,c53,c54,c55,c56,c57,c58,c59,c60,c61,c62,c63,c64,c65,c66,c67,c68,c69,c70,c71,c72,c73,c74,c75,c76,c77,c78,c79,c80,c81,c82,c83,c84,c85,c86,c87,c88,c89,c90,c91,c92,c93,c94,c95,c96,c97,c98,c99,c100,c101,c102,c103,c104,c105,c106,c107,c108,c109,c110,c111,c112,c113,c114,c115,c116,c117,c118,c119,c120,c121,c122,c123,c124,c125,c126,c127,c128,c129,c130,c131,c132,c133,c134,c135,c136,c137,c138,c139,c140,c141,c142,c143,c144,c145,c146,c147,c148,c149,c150,c151,c152,c153,c154,c155,c156,c157,c158,c159,c160,c161,c162,c163,c164,c165,c166,c167,c168,c169,c170,c171,c172,c173,c174,c175,c176,c177,c178,c179,c180,c181,c182,c183,c184,c185,c186,c187,c188,c189,c190,c191,c192,c193,c194,c195,c196,c197,c198,c199,c200,c201,c202,c203,c204,c205,c206,c207,c208,c209,c210,c211,c212,c213,c214,c215,c216,c217,c218,c219,c220,c221,c222,c223,c224,c225,c226,c227,c228,c229,c230,c231,c232,c233,c234,c235,c236,c237,c238,c239,c240,c241,c242,c243,c244,c245,c246,c247,c248,c249,c250,c251,c252,c253,c254,c255);
      subtype Byte is ByteT; --well, maybe I oversubtyped it, hehe
      type ByteFileType is file of Byte;

      file outfile : ByteFileType open write_mode is "image.jpg";
      variable Pixel : Byte;
   	variable bufer: LINE;
      variable Header : std_logic_vector(4855 downto 0) := X"FFD8FFE000104A46494600010200000100010000FFC00011080020004003012200021101031101FFDB008400100B0C0E0C0A100E0D0E1211101318281A181616183123251D283A333D3C3933383740485C4E404457453738506D51575F626768673E4D71797064785C656763011112121815182F1A1A2F634238426363636363636363636363636363636363636363636363636363636363636363636363636363636363636363636363636363FFC401A20000010501010101010100000000000000000102030405060708090A0B100002010303020403050504040000017D01020300041105122131410613516107227114328191A1082342B1C11552D1F02433627282090A161718191A25262728292A3435363738393A434445464748494A535455565758595A636465666768696A737475767778797A838485868788898A92939495969798999AA2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FA0100030101010101010101010000000000000102030405060708090A0B1100020102040403040705040400010277000102031104052131061241510761711322328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728292A35363738393A434445464748494A535455565758595A636465666768696A737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00";
      --pragma translate_on

   begin
      if (reset = '1') then
         CompressingInt <= '0';
         Linea := (others => '0');
         Columna := (others => '0');
         Bloque := "00";
         StepV <= 0;
         weY2 <= '0';
         weCb2 <= '0';
         weCr2 <= '0';
         Base := (others => '0');
         BaseH := (others => '0');
         BaseQ := (others => '0');        
         DCTQ := (others => '0');
         DIND <= (others => '0');
         ND <= '0';
         --Huffmanear <= '0';
         PrevDC := (others => '0');
         Coeficiente := 0;
         HuffmanWord := (others => '0');
         HuffmanWordPos := 22; --first free LSB (HuffmanWord's MSB)
         Elemento := "00";
         IniDC := '1';
         GetPrevDC := '1';
         FirstDC := '1';
         Save <= '0';
         addri <= "0000000000101100"; --first Table
         addribk <= "0000001001011110"; --pointer to last Header byte
         addrH <= (others => '0');
         addrQ <= (others => '0');
         addrCb2 <= (others => '0');
         addrCr2 <= (others => '0');
         addrY2 <= (others => '0');
         din <= (others => '0');
         we <= '0';
         dinY2 <= (others => '0');
         dinCr2 <= (others => '0');
         dinCb2 <= (others => '0');
         Done <= '0';
         Cat := 0;
         ZRL := 0;
         Coef := (others => '0');
         ColBk := (others => '0');
         LinBk := (others => '0');
         Sign := '0';
         ZeroRun := 0;
         Primera := '0';
         LumaBlock := '0';
         NDe <= '0';           
         HeaderFinal := '0';
         DatoHeader := 0;
         VarTamImg := (others => '0');    
         WriteAdditionalBits <= '0';
         TempCompDC := (others => '0');
         WriteZRL := '0';
         LastBlockDCY := (others => '0');
         LastBlockDCCb := (others => '0');
         LastBlockDCCr := (others => '0');            
         AddVal := (others => '0');                
         QDC := (others => '0');
         WriteTables <= '0';
         TableData <= (others => '0');
         Table <= '0';    
         addrTablaQ <= (others => '0');
         ZRLing <= '0';       
         RFDInt <= '0';
         RFDIntData <= (others => '0');
      elsif (clk = '1' and clk'event) then
         if CompressImage = '1' then
            FirstDC := '1';
            IniDC := '1';
            HeaderFinal := '0';
            GetPrevDC := '1';
            PrevDC := (others => '0'); --initialized to zero at the beginning of the image
            HuffmanWord := (others => '0');
            HuffmanWordPos := 22;
            Save <= '0';
            DatoHeader := 0;
            WriteAdditionalBits <= '0';
            HeaderFinal := '0';
            WriteTables <= '1';
            TableData <= (others => '0');
            Table <= '0';
            addribk <= "0000001001011110"; --pointer to the Header's last byte
            addri <= "0000000000101100"; --first Table
            case Compression is
               when "00" => --low quality
                  BaseQ := "100000000";
               when "01" => --medium quality
                  BaseQ := "010000000";
               when others => --10 high quality
                  BaseQ := "000000000";
            end case;
            addrTablaQ <= BaseQ; --ready first data

            CompressingInt <= '0';
            Bloque := "00";
            StepV <= 0;
            weY2 <= '0';
            weCb2 <= '0';
            weCr2 <= '0';
            Base := (others => '0');
            BaseH := (others => '0');
            ND <= '0';
            Coeficiente := 0;
            we <= '0';
            Done <= '0';            
            ZRLing <= '0';
            RFDInt <= '0';
            RFDIntData <= (others => '0');
            --pragma translate_off
               Columna := ('0' & ImgLines) + 1;                  
               for i in 606 downto 563 loop
                  case i is
                     when (606-25) => --MSByte of size Y
                        Pixel := ByteT'val(conv_integer("0000000" & Columna(8)));
                     when (606-26) => --LSByte of size Y
                        Pixel := ByteT'val(conv_integer(Columna(7 downto 0)));
                        Columna := ImgColumns + 1;
                     when (606-27) => --MSByte of size X
                        Pixel := ByteT'val(conv_integer("000000" & Columna(9 downto 8)));
                     when (606-28) => --LSByte of size X
                        Pixel := ByteT'val(conv_integer(Columna(7 downto 0)));
                     when others =>
                        Pixel := ByteT'val(conv_integer(Header(8*i+7 downto 8*i)));
                  end case;      
                  write(outfile, Pixel);
               end loop;            
            --pragma translate_on
            
         end if;
         we <= '0'; --after the ones that rise it
         if WriteTables = '1' then --write in memory the quantization tables for the selected compression level
            if Table = '0' then --the start of Table Q of value "00h" is in position 2Bh (43=101011)
               addri <= "0000000000101100" + ('0' & TableData);
            else--the start of Table Q of value "01h" is in position 6Ch (108=1101100) 
               addri <= "0000000001101101" + ('0' & TableData);
            end if;   
            din <= doutTablaQ;
            addrTablaQ <= addrTablaQ + 1;
            
            if addrTablaQ /= BaseQ then --first coefficient, not written because we'll have it also in next cycle
               --until second coefficient is loaded
               we <= '1';
               TableData <= TableData + 1;
               --pragma translate_off
               Pixel := ByteT'val(conv_integer(doutTablaQ));
               write(outfile, Pixel);
               --pragma translate_on            
            end if;
            
            if TableData = "111111" then
               if Table = '1' then --we are over
                  --pragma translate_off
                  for i in 433 downto 0 loop
                     Pixel := ByteT'val(conv_integer(Header(8*i+7 downto 8*i)));
                     write(outfile, Pixel);
                  end loop;            
                  --pragma translate_on                  

                  WriteTables <= '0';
                  addrTablaQ <= (others => '0'); --so that it doesn't stay in a non-existent address
               else   
                  Table <= '1';                 
                  --pragma translate_off
                  Pixel := ByteT'val(1); --points to the start of Table 01
                  write(outfile, Pixel);
                  --pragma translate_on                              
               end if;   
            end if;   
         end if;   
         
         if (MakeDCT = '1' or CompressingInt = '1') then --MakeDCT lasts only one cycle, but CompressingInt is high the following ones
            if MakeDCT = '1' then
               HeaderFinal := '0';
               --write the image size, now that we know it, in the header as at the least we must do
               --4 DCTs in an image, there won't be any problem if image size writing is done in four steps
               --or if it is repeated throughout the image's processing
               case DatoHeader is
                  when 0 => --MSByte of ImgLines+1
                     addri <= "0000000000011001"; --position 0 of the two bytes of size Y (lines)
                     we <= '1';
                     VarTamImg := ("00" & ImgLines) + 1;
                     din <= "000000" & VarTamImg(9 downto 8);
                     DatoHeader := 1;
                  when 1 => --LSByte of ImgLines+1
                     addri <= "0000000000011010"; 
                     we <= '1';
                     din <= VarTamImg(7 downto 0);
                     DatoHeader := 2;
                  when 2 => --MSByte of ImgColumns+1
                     addri <= "0000000000011011"; --position 0 of the two bytes of size X (columns)
                     we <= '1';
                     VarTamImg := ('0' & ImgColumns) + 1;
                     din <= "00000" & VarTamImg(10 downto 8);
                     DatoHeader := 3;
                  when 3 => --LSByte of ImgColumns+1
                     addri <= "0000000000011100"; 
                     we <= '1';
                     din <= VarTamImg(7 downto 0);
                     DatoHeader := 4;
                  when 4 to 7 => --it's been already written, don't do anything(we put 8 DatoHeader because in the last
                     --block when we pass to the next image, ImgLines is zero and it is wrongly written)
                     we <= '0';
                     if DatoHeader = 7 then
                        DatoHeader := 0;
                     else
                        DatoHeader := DatoHeader + 1;
                     end if;   
               end case;
               CompressingInt <= '1';
               if ColumnToCompress = "0000000000" then --we've changed line, careful
                  --because we have to process the last block of the previous line
                  Linea := (not LineToCompress(3)) & "000";
                  Columna := ImgColumns(9 downto 3) & "000";
               else
                  Linea := LineToCompress(3) & "000";
                  Columna := (ColumnToCompress(9 downto 3) - 1) & "000";
               end if;
               Bloque := ColumnToCompress(1 downto 0);
               StepV <= 0;
               weY2 <= '0';
               weCb2 <= '0';
               weCr2 <= '0';
               Coeficiente := 0;
               Save <= '0';
               IniDC := '1';           
               GetPrevDC := '1';
               WriteAdditionalBits <= '0';
            else
            
            we <= '0';
            LumaBlock := Bloque(1) nor Bloque(0); --it is 1 if we are dealing with luminance component
            --It is necessary that Base be here so that it changes when Bloque changes in the "if" of Done in the last image's block
            if Bloque = "00" then --Q Coefficients of Table 0 (luminance)
               BaseH := (others => '0'); --address 0 of Huffman Luminance Table
               case Compression is
                  when "00" => --Low Quality
                     Base := "100000000";
                  when "01" => --Medium Quality
                     Base := "010000000";
                  when others => --10 High Quality
                     Base := "000000000";
               end case;
            else --Q Coefficients of Table 1 (chroma)
               BaseH := "010110000"; --address 0 of Huffman Chrominance Table
               case Compression is
                  when "00" => --Low Quality
                     Base := "101000000";
                  when "01" => --Medium Quality
                     Base := "011000000";
                  when others => --10 High Quality
                     Base := "001000000";
               end case;               
            end if;   
            
            case StepV is
               when 0 => --with the first data to let addrXX2 load and have the data in memory
                  weY2 <= '0';
                  weCb2 <= '0';
                  weCr2 <= '0';
                  if CompressingInt = '1' and Done = '0' then --Done=0 cause in case it is a Y block (but not the fourth (the last)) coming from StepV=3
                     StepV <= 1;
                  else
                     StepV <= 0;
                  end if;   
                  NDe <= '0';
                  RFDInt <= '0';
               when 1 => --Feeding of DCT1 with 64 pixels
                  if RFD = '1' then --DCT1 ready for data (RFD)
                     NDe <= '1';
                     ND <= NDe; --this way we delay ND one cycle to synchronize data
                     if RFDInt = '0' then
                        case Bloque is
                           when "00" => --block Y
                              DIND <= doutY(7 downto 0); 
                           when "01" => --block Cb
                              DIND <= doutCb(7 downto 0); 
                           when others => --can only be block Cr
                              DIND <= doutCr(7 downto 0); 
                        end case;
                     else
                        DIND <= RFDIntData; --to recover the "lost" cycle because RFD was 0 (if the DCT was such that RFD were 0 for more than a cycle it would be necessary to change this part)
                        RFDInt <= '0';
                     end if;   
                     if Columna(2 downto 0) = "111" then
                        Columna := Columna(9 downto 3) & "000";
                        Linea := Linea + 1;
                        if Linea(2 downto 0) = "000" then --we've finished this StepV, now we'll wait for the results
                           StepV <= 2; --we shall go to StepV 2 with the first column of the block
                           Linea(3) := not Linea(3); --makes it rest in current block
                           Primera := '1'; --to not let StepV 2 change Linea or Column in its 1st cycle because they already have adequate values
                        end if;   
                     else
                        Columna := Columna + 1;
                     end if;   
                  else
                     --to not lose the just read pixel (rememer that reads are ahead of their time!!)
                     RFDInt <= '1';
                     case Bloque is
                        when "00" => --block Y
                           RFDIntData <= doutY(7 downto 0); 
                        when "01" => --block Cb
                           RFDIntData <= doutCb(7 downto 0); 
                        when others => --can only be block Cr
                           RFDIntData <= doutCr(7 downto 0); 
                     end case;
                     --ND <= '0';
                  end if;
                  weY2 <= '0';
                  weCb2 <= '0';
                  weCr2 <= '0';
                  addrQ <= Base; --DC Coefficient's Q (Base + Linea*8 + Columna)
                  QDC := (others => '0');
               when 2 => --we receive the data, quantize it, truncate it and save them back to the buffer
                  --watch out because DCT1 outputs data column-wise (feeding was row-wise)
                  NDe <= '0'; --it stayed high while sending data byte
                  ND <= NDe;
                  case Bloque is  --needed to send the last data of the block (it is alright!!!)
                     when "00" => --block Y
                        DIND <= doutY(7 downto 0); 
                     when "01" => --block Cb
                        DIND <= doutCb(7 downto 0); 
                     when others => --can only be block Cr
                        DIND <= doutCr(7 downto 0); 
                  end case;                  
                  
                  if RDY = '1' then --output data ready!!
                     if QDC /= 0 then
                        DCTQ := MultiplierQ(DOUTD(14 downto 3), QDC);
                        QDC := (others => '0');
                     else        
                        DCTQ := MultiplierQ(DOUTD(14 downto 3), DOUTQ); --multiply the DCT coefficient times Qyx fraction's numerator
                     end if;
                     --if DCTQ(24) = '0' then --it is positive, let's round
                        DCTQ(24 downto 14) := DCTQ(24 downto 14) + DCTQ(13); --round to nearest integer
                     --end if;   
                     --if DCTQ(24 downto 13) = "111111111111" then --it is less or equal than -0.5, roung to 0
                     --   DCTQ(24 downto 13) := "000000000000"; --or else it will be read as -1
                     --end if;
                     case Bloque is --the "+ DCTQ(13)" is the nearest integer round!
                        when "00" => --block Y
                           dinY2 <= DCTQ(24) & DCTQ(24 downto 14);-- + DCTQ(13); --we divide (shifting right) by the general Q coefficient denominator (16384) and save the quantized coefficient
                           --sign-extension
                           weY2 <= '1';
                        when "01" => --block Cb
                           dinCb2 <= DCTQ(24) & DCTQ(24 downto 14);-- + DCTQ(13); --we divide (shifting right) by the general Q coefficient denominator (16384) and save the quantized coefficient
                           weCb2 <= '1';
                        when others => --can only be block Cr
                           dinCr2 <= DCTQ(24) & DCTQ(24 downto 14);-- + DCTQ(13); --we divide (shifting right) by the general Q coefficient denominator (16384) and save the quantized coefficient
                           weCr2 <= '1';
                     end case;                     
                     
                     --write column-wise
                     if Linea(2 downto 0) = "111" and Primera = '0' then --have we reached this line's end?
                        --Primera is used for the first cycle of StepV2 in which Columna and Linea arrive with the right value
                        --so they need not be changed in order to get the correct address
                        Linea := Linea(3) & "000";
                        Columna := Columna + 1;
                     else
                        if Primera = '1' then
                           Primera := '0';
                        else   
                           Linea := Linea + 1;
                           if Linea(2 downto 0) = "111" and Columna(2 downto 0) = "111" then --we have finished this StepV, now we must Huffman encode 
                              StepV <= 3;
                           end if;   
                        end if;   
                     end if;
                     
                     --the following code is because we must load one cycle early the addrQ so that in the
                     --current cycle we can have the right Q coefficient for current Linea and Columna values
                     if Linea(2 downto 0) = "110" then --have we reached the end?
                        --next coeff. is Linea 0 of the Coeff. Table and Columna is current plus one
                        addrQ <= Base + Columna(2 downto 0) + 2; --Base + Linea*8 + Columna --> remember coefficient table is 8x8
                     else
                        --next coeff is next Linea of the Coeff. Table and current Columna
                        addrQ <= Base + ((Linea(2 downto 0) + 2) & "000") + Columna(2 downto 0); --Base + Linea*8 + Columna
                     end if;
                     
                  else
                     --during Latency cycles we will be here waiting until RDY is 1
                     --and also every X cycles when RDY becomes 0 for one cycle
                     if QDC = 0 and addrQ = Base then --only happens when we enter this StepV from the previous one
                        QDC := DOUTQ; --save the DC value because Q reading must be ahead by two cycles now
                        addrQ <= Base + "1000"; --Base + Linea*8 + Columna (Linea=1, Columna=0)
                     end if;   
                     weY2 <= '0';
                     weCr2 <= '0';
                     weCb2 <= '0';
                  end if;   
               when 3 =>
                  weY2 <= '0';
                  weCr2 <= '0';
                  weCb2 <= '0';
                  --Make the "pointers" Linea and Columna point to the beginning of the next block
                  --must do it here because in the last cycle of StepV2, when StepV becomes 3, Linea and Columna
                  --must maintain their value
                  Linea := Linea(3) & "000";
                  Columna := Columna + 1;
                  if LumaBlock = '1' then --if we are processing the luminance block we can only save it when we have processed the 4 ones
                     --that compose the 2x2 block of subsampling, there is no problem with the order of the chroma ones because these ones too
                     --are saved when we have averaged four and that is controlled by the process RGB2YCbCr
                     if Linea(3) = '1' and Columna(3) = '0' then --checked, it is alright (remember it points to the next block)
                        StepV <= 4;
                        Elemento := "00"; --process the first square (8x8 block) of the 2x2 luminance block (16x16 pixels, 2x2 squares)
                        --Ready Linea and Columna to read element 00 that Huffman will receive
                        Linea := (others => '0');
                        Columna := (Columna(9 downto 4) - 1) & "0000";
                     else --the three first squares of the 2x2 luminance block are not sent to Huffman yet (subsampling requirements)
                        StepV <= 0;
                        Done <= '1';
                     end if;
                  else
                     Columna := Columna - 1;
                     Columna := Columna(9 downto 3) & "000"; --it is alright, because Columna arrives already adjusted to Cb and Cr.
                     StepV <= 4;
                  end if;
                  WriteAdditionalBits <= '0';
               when 4 => --with this dummy cycle we give time to the memory to give us address 0
                  --for the DC coefficient
                  weY2 <= '0';
                  weCr2 <= '0';
                  weCb2 <= '0';
                  Save <= '0'; --just in case we left it high in last step
                  we <= '0';
                  StepV <= 5;  
               when 5 => --Linea and Columna point to the beginning of the block

                  --If we change Linea and/or Columna in cycle 0 of this section, then the new address will
                  --be sent to memory in cycle 1 and in cycle 2 doutX will have the asked data, so careful!!
                  case Bloque is
                     when "00" => --block Y
                        Coef := doutY; 
                     when "01" => --block Cb
                        Coef := doutCb; 
                     when others => --can only be block Cr
                        Coef := doutCr; 
                  end case;

                  
                  we <= '0';   
                  if Done = '0' then --this way it does not go to look for the DC when we change Block (upsetting FirstDC)
                  if Save = '0' then
                     if WriteAdditionalBits = '0' then
                        if Coeficiente = 0 then 
                           --The previous component is read from the buffer varying Linea and Columna
                           if IniDC = '1' then --we've not yet obtained the previous DC to calculate the difference
                              if Linea = "0000" and Columna = "000000000" and FirstDC = '1' and GetPrevDC = '1' then --must do it this way
                                 --or else the luminance, which has 4 blocks in its first Huffmanear takes DC=0 for all
                                 --GetPrevDC = 1 is so that execution doesnt get here if in the previous cycle GetPrevDC became zero in the bottom "if"
                                 --and put Linea and Columna to zero
                                 --we are in the first block of the image, PrevDC is zero
                                 if Bloque = "10" then --if we are in the last, we zero it
                                    FirstDC := '0'; --because it has already been used by the three components (Y,Cb,Cr)
                                 end if;
                                 PrevDC := (others => '0');
                                 IniDC := '0';
                              else
                                 if GetPrevDC = '1' then
                                    ColBk := Columna; --save the values of Linea and Columna
                                    LinBk := Linea;
                                    
                                    --Must obtain the quantized DC coefficient of the last processed block
                                    --as stated by specification, but actually the last block is indicated by MCU
                                    if Columna(9 downto 3) = "0000000" then --first block of the row?
                                       if LumaBlock = '1' then
                                          if Elemento = "00" then
                                             --Remember that DCTs are done when only the last line of all is left to be written in the block
                                             --so the DC of the previous block has already been overwritten, that's why we use LastBlockDCY
                                             PrevDC := LastBlockDCY;
                                             IniDC := '0'; --this way we skip the next StepV
                                          else --Elemento 10 need the DC of Elemento 01
                                             Linea(3) := '0';                                             
                                             Columna(3) := '1';
                                          end if;   
                                       else --in chrominance ImgColumns has half the image's columns!
                                          if Bloque = "01" then --block Cb
                                             PrevDC := LastBlockDCCb;
                                          else --10 = block Cr
                                             PrevDC := LastBlockDCCr;
                                          end if;
                                          IniDC := '0';
                                       end if;   
                                    else
                                       if LumaBlock = '1' then
                                          case Elemento is
                                             when "00" => --DC of previous element 11
                                                Columna := (Columna(9 downto 3)-1) & "000";
                                                Linea(3) := '1';
                                             when "01" => --DC of element 00
                                                Columna(3) := '0';
                                                Linea(3) := '0';
                                             when "10" => --DC of element 01
                                                Columna(3) := '1';
                                                Linea(3) := '0';
                                             when others => --11 DC of element 10
                                                Columna(3) := '0';
                                                Linea(3) := '1';
                                          end case;      
                                       else                                             
                                          Columna := (Columna(9 downto 3) - 1) & "000"; --first column of previous block
                                       end if;   
                                    end if;
                                    StepV <= 4; --wait one cycle for PrevDC
                                    GetPrevDC := '0';
                                 else   
                                    GetPrevDC := '1'; --we rise it for the next block
                                    IniDC := '0';
                                    PrevDC := Coef;
                                    Linea := LinBk;
                                    Columna := ColBk;
                                    StepV <= 4; --wait one cycle to get back the current DC component to operate with it
                                 end if;                        
                              end if;
                           else --IniDC = '0' that is, we now have the PrevDC  
                              GetPrevDC := '1'; --we rise it for the second block after the first one of the image
                              --in this "if" we see if this is the last block in the buffer and in that case
                              --save its DC component
                              if LumaBlock = '1' then
                                 if Columna(9 downto 3) = ImgColumns(9 downto 3) and Elemento="11" then
                                    LastBlockDCY := Coef;
                                 end if;
                              else
                                 if Columna(9 downto 3) = ImgColumns(9 downto 4) then
                                    if Bloque = "01" then --block Cb
                                       LastBlockDCCb := Coef;
                                    else --block Cr
                                       LastBlockDCCr := Coef;
                                    end if;
                                 end if;
                              end if;
                              
                              Coef := Coef - PrevDC; --this IS DIFF
                              --And I cite the standard: "When DIFF is negative, the SSSS low order bits
                              --  of (DIFF-1) are appended. Note that the MSB of the appended bit sequence is 0
                              --  for negative differences and 1 for positive differences."
                              -- So it was this s***** paragraph's misreading the one that got me a good headache

                              Sign := not Coef(11); --the first additional bit after the category is this one
                              Cat := GetCategory(Coef); --we send Coeficiente and it returns its category SSSS
                              if Coef(11) = '1' then --it is negative, must write DIFF-1 (JPEG standard requirement)
                                 AddVal := Coef - 1; --must use AddVal because Coef is overwritten every cycle
                              else --it is positive, must write DIFF
                                 AddVal := Coef; --must use AddVal because Coef is overwritten every cycle
                              end if;
                              
                              TempCompDC := CompressDC(Cat, LumaBlock);
                              HuffmanWord := AppendHuffmanWordL (HuffmanWord, TempCompDC,conv_integer(TempCompDC(14 downto 11))+1, HuffmanWordPos);
                              HuffmanWordPos := HuffmanWordPos - conv_integer(TempCompDC(14 downto 11)) - 1;
                              --pragma translate_off   
                              --the following lines are for debugging, comment them out at will
                              case Bloque is
                                 when "00" => WRITELINE(DebugY,bufer);
                                 when "01" => WRITELINE(DebugCb,bufer);
                                 when others => WRITELINE(DebugCr,bufer);
                              end case;
                              WRITE(bufer,now);WRITE(bufer,espacios);
                              WRITE(bufer,strLinea);WRITE(bufer,Linea);WRITE(bufer,espacios);
                              WRITE(bufer,strColumna);WRITE(bufer,Columna);
                              case Bloque is
                                 when "00" => WRITE(bufer,espacios);WRITE(bufer,strElemento);WRITE(bufer,Elemento);WRITELINE(DebugY,bufer);
                                 when "01" => WRITELINE(DebugCb,bufer);
                                 when others => WRITELINE(DebugCr,bufer);
                              end case;
                              WRITE(bufer,TempCompDC(conv_integer(TempCompDC(14 downto 11)) downto 0));WRITE(bufer,espacio);
                              --pragma translate_on                              
                              Save <= '1';
                              WriteAdditionalBits <= '1';
                           end if;
                        else --Coeficiente /= 0
                           if Coef = "000000000000" then
                              ZeroRun := ZeroRun + 1; --if we get to 16 then we have to write a F/0 (ZRL)
                              if ZeroRun = 16 then --but we can not write it unless we find later a non-zero AC coefficient
                                 --because if all the following coefficients are zero we have to write instead and EOB (End Of Block)
                                 ZRL := ZRL + 1;
                                 ZeroRun := 0;
                              end if;
                              if Coeficiente = 63 then
                                 Save <= '1';
                              else   
                                 Coeficiente := Coeficiente + 1;
                                 StepV <= 4; --to give time to load the next coefficient
                              end if;   
                           else --the coefficient is not zero
                              if ZRL > 0 then
                                 --must write in HuffmanWord all the previous ZRLs
                                 Save <= '1'; --before writing the data of the current non-zero coefficient
                                 ZRLing <= '1'; --so that it doesn't skip the current coefficient when it finishes writing the preceding ZRLs
                              else
                                 if addrH = "000000000" then --this way we know if we still have to ask the ROM for the Huffman code
                                    Sign := not Coef(11); --the first addditional bit after category is this one
                                    Cat := GetCategory(Coef); --we send the coefficient and it returns the category SSSS
                                    if Coef(11) = '1' then --it is negative, we must write DIFF-1 (JPEG standard requirement)
                                       AddVal := Coef - 1; --we have to use AddVal because Coef is overwritten every cycle
                                    else --it is positive, we must write DIFF
                                       AddVal := Coef; --we have to use AddVal because Coef is overwritten every cycle
                                    end if;                              
                                    --Reading of Huffman_ROM to obtain the code of the coefficient and add it to HuffmanWord
                                    --ZeroRun and Cat tell us which code we are looking for
                                    addrH <= BaseH + To_std_logicvpor11(ZeroRun) + ("00000" & To_std_logicv(Cat));
                                    ZeroRun := 0; --of couse, once used it must be zeroed!
                                    StepV <= 4; --this way we give time to load the Huffman value from memory
                                 else --now we have the Huffman code that the Huffman_ROM gives us and can now write it
                                    --remember that the 4 MSBs of this code mean the length of the code in the 16 LSBs minus one!!
                                    Hlength := conv_integer(doutH(19 downto 16));
                                    HuffmanWord := AppendHuffmanWordL (HuffmanWord, doutH, Hlength+1, HuffmanWordPos);
                                    HuffmanWordPos := HuffmanWordPos - Hlength - 1;
                                    --pragma translate_off
                                    WRITE(bufer,doutH(conv_integer(doutH(19 downto 16)) downto 0));WRITE(bufer,espacio);
                                    --pragma translate_on
                                    Save <= '1';
                                    WriteAdditionalBits <= '1';
                                 end if;   
                              end if;
                           end if;                     
                        end if; --if Coeficiente = 0                  
                     else --WriteAdditionalBits = '1'
                        --we can now write the Additional Bits, starting by the sign
                        case Cat is
                           when 0 =>
                              null;
                           when 1 =>
                              HuffmanWord := AppendHuffmanWord (HuffmanWord, Sign, HuffmanWordPos);
                              HuffmanWordPos := HuffmanWordPos - 1;
                              --pragma translate_off
                              WRITE(bufer,Sign);
                              --pragma translate_on
                           when others =>
                              HuffmanWord := AppendHuffmanWord (HuffmanWord, Sign, HuffmanWordPos);
                              HuffmanWordPos := HuffmanWordPos - 1;
   
                              --Coef := GetMagnitude(Coef, Cat);
                              HuffmanWord := AppendHuffmanWordL (HuffmanWord, AddVal, Cat-1, HuffmanWordPos);
                              HuffmanWordPos := HuffmanWordPos - Cat + 1;

                              --pragma translate_off
                              WRITE(bufer,Sign);WRITE(bufer,espacio);
                              WRITE(bufer,AddVal(Cat-2 downto 0));
                              --pragma translate_on
                        end case;
                        --pragma translate_off
                        WRITE(bufer,puntoycoma);
                        --pragma translate_on
                        addrH <= (others => '0'); --this way we know if we have obtained the Huffman code and may save it
                        Save <= '1';
                        WriteAdditionalBits <= '0';
                     end if; --if WriteAdditionalBits
                     
                  else --Save = '1'
                     if (ZRL > 0 or (ZeroRun /=0 and Coeficiente=63 and Coef="000000000000")) and WriteZRL = '0' then --there can be at most 3 ZRLs in a block
                        --first we have to write the ZRL F/0 that lie between the current coefficient and the last non-zero
                        --so that these get written in HuffmanWord before the AC coefficient that follows them
                        --in the previous "if", in which we write the coefficients, when we find one non-zero and ZRL>0
                        --the first thing it will do is to rise Save to come here to fill HuffmanWord without augmenting Coeficiente
                        if Coeficiente = 63 and Coef="000000000000" then --there is no need to write the ZRLs, only EOB (which means all to follow are zeros)
                           if LumaBlock = '1' then --EOB of luminance
                              HuffmanWord := AppendHuffmanWord (HuffmanWord, "1010", HuffmanWordPos);
                              HuffmanWordPos := HuffmanWordPos - 4;
                              --pragma translate_off
                              WRITE(bufer, string'("1010 (EOB)"));
                              --pragma translate_on                              
                           else --EOB of chrominance
                              HuffmanWord := AppendHuffmanWord (HuffmanWord, "00", HuffmanWordPos);   
                              HuffmanWordPos := HuffmanWordPos - 2;                
                              --pragma translate_off
                              WRITE(bufer, string'("00 (EOB)"));
                              --pragma translate_on                                                            
                           end if;                    
                           --pragma translate_off
                           case Bloque is
                              when "00" => WRITELINE(DebugY,bufer);
                              when "01" => WRITELINE(DebugCb,bufer);
                              when others => WRITELINE(DebugCr,bufer);
                           end case;
                           --pragma translate_on
                           ZRL := 0;
                           ZeroRun := 0;
                        else  --if else, we write ZRLs until ZRL=0
                           if LumaBlock = '1' then
                                 HuffmanWord := AppendHuffmanWord (HuffmanWord, "11111111001", HuffmanWordPos);
                                 HuffmanWordPos := HuffmanWordPos - 11;
                                 --pragma translate_off
                                 WRITE(bufer, string'("11111111001 (ZRL)"));WRITE(bufer,puntoycoma);
                                 --pragma translate_on
                           else
                                 HuffmanWord := AppendHuffmanWord (HuffmanWord, "1111111010", HuffmanWordPos);
                                 HuffmanWordPos := HuffmanWordPos - 10;
                                 --pragma translate_off
                                 WRITE(bufer, string'("1111111010 (ZRL)"));WRITE(bufer,puntoycoma);
                                 --pragma translate_on                                 
                           end if;
                           ZRL := ZRL - 1;
                           WriteZRL := '1';
                        end if;
                     else --ZRL=0
                        --Code to save in buffer_img whatever we have in HuffmanWord in several cycles, one byte at a time,
                        --if necessary, until HuffmanWordPos becomes >14 (REMEMBER that this variable points to the
                        --first empty LSB! so that HuffmanWord will be occupied with data in (22 downto HuffmanWordPos+1))
                        if HuffmanWordPos < 15 then --there are still byte/s to save
                           if addribk < MaxImageSize then --the buffer size, so that it doesn't become 0 and overwrite the Header
                              we <= '1';
                              if HeaderFinal = '1' and HuffmanWordPos = 14 then --last byte to write in the file
                                 --is +2 because +1 is the last byte of info for address (starts at address 0!) and plus 2 is
                                 --the file size starting from 1
                                 addri <= addribk + 2; --at the end, addr will be addri and its value will be the JPEG compressed image size in bytes
                              end if;                           
                              addribk <= addribk + 1; --always add 1, the writing of the Header will leave it pointing
                              --at the last byte of the Header so that there won't be any problem
                              --and at the end of all the compression, addr will be the numerical size of the image because
                              --it will point at the last position of information plus one (remember data starts at 0)
                              din <= HuffmanWord(22 downto 15);
                              --pragma translate_off
                              Pixel := ByteT'val(conv_integer(HuffmanWord(22 downto 15)));
                              write(outfile, Pixel);
                              if HeaderFinal = '1' and HuffmanWordPos = 14 then
                                 File_Close(outfile);
                              end if;
                        		--pragma translate_on 
                           end if;   


                           if HuffmanWord(22 downto 15) = "11111111" and HeaderFinal = '0' then --if we ever write FFh it must be followed by 00h
                              --but in the case of the EOI marker or we will spoil it!
                              HuffmanWord := "00000000" & HuffmanWord(14 downto 0);
                           else   
                              HuffmanWord := HuffmanWord(14 downto 0) & "00000000";
                              HuffmanWordPos := HuffmanWordPos + 8; --free up the 8 written LSBs
                           end if;
                        else --HuffmanWordPos > 14 --can't save more
                           if WriteZRL = '0' then
                           if Coeficiente = 63 and HeaderFinal = '0' and Bloque = "10" and (LineAbsToCompress > ImgLines) then --last block of all
                              --must write whatever remains in HuffmanWord and, if not byte aligned, then make one-padding
                              --and write EOI marker FFD9h
                              if HuffmanWordPos /= 22 then
                                 HuffmanWord := AppendHuffmanWord (HuffmanWord, "11111111", HuffmanWordPos);
                                 --we write eight ones just to make sure, the extra ones will get overwritten in next cycle by the EOI header
                                 --because we make HuffmanWordPos point the first byte:
                                 HuffmanWordPos := 14; --first free LSB
                              else --we make first the 1 bit padding and then later the EOI header to not saturate the HuffmanWord variable
                                 HuffmanWord := AppendHuffmanWord (HuffmanWord, "1111111111011001", HuffmanWordPos);
                                 HuffmanWordPos := HuffmanWordPos - 16; --first free LSB
                                 HeaderFinal := '1';
                                 --pragma translate_off
                                 case Bloque is
                                    when "00" => WRITELINE(DebugY,bufer);
                                    when "01" => WRITELINE(DebugCb,bufer);
                                    when others => WRITELINE(DebugCr,bufer);
                                 end case;
                                 --pragma translate_on
                              end if;   
                           else
                              Save <= '0';
                              if ZRLing = '1' then --if not here, it will skip one coefficient after writing the previous ZRLs
                                 ZRLing <= '0'; --because WriteZRL will be 0, ZRL also and WriteAdditionalBits will be 0 and Coeficiente /=63
                              else   
                                 if WriteAdditionalBits = '0' then
                                    if Coeficiente = 63 then --we can only get to Coefficient 63 from here if we have processed the whole block
                                       Coeficiente := 0; --for the next block
                                       IniDC := '1';
                                       if LumaBlock = '1' then
                                          --must process the different elements
                                          Elemento := Elemento + 1;
                                          case Elemento is
                                             when "00" => --we've finished the 2x2 block!!
                                                Done <= '1';
                                             when "01" =>
                                                Columna(3) := '1';
                                                Linea(3) := '0';
                                             when "10" =>
                                                Columna(3) := '0';
                                                Linea(3) := '1';
                                             when others => --11
                                                Columna(3) := '1';
                                                Linea(3) := '1';
                                          end case;      
                                       else
                                          Done <= '1';
                                       end if;               
                                    else
                                       Coeficiente := Coeficiente + 1;
                                       StepV <= 4; --to give time to the next coefficient to load from mem
                                    end if;
                                 end if;
                              end if;   
                           end if;
                           end if;
                           if WriteZRL = '1' then
                              WriteZRL := '0';
                           end if;  
                        end if; --if HuffmanWordPos < 15   
                     end if; --if ZRL > 0
                  end if;
                  end if;
                  
                  --the place for the following cases is here because Coeficiente may be changed from two places, after Save and
                  --after finding a 00h Coefficient
                  --These are to prepare the reading (in zigzag order) of the next coefficient from memory
                  case Coeficiente is
                     when 0|1|5|6|14|15|27|28 =>
                        Linea(2 downto 0) := "000";
                     when 2|4|7|13|16|26|29|42 =>
                        Linea(2 downto 0) := "001";
                     when 3|8|12|17|25|30|41|43 =>
                        Linea(2 downto 0) := "010";
                     when 9|11|18|24|31|40|44|53 =>
                        Linea(2 downto 0) := "011";
                     when 10|19|23|32|39|45|52|54 =>
                        Linea(2 downto 0) := "100";
                     when 20|22|33|38|46|51|55|60 =>
                        Linea(2 downto 0) := "101";
                     when 21|34|37|47|50|56|59|61 =>
                        Linea(2 downto 0) := "110";
                     when 35|36|48|49|57|58|62|63 =>
                        Linea(2 downto 0) := "111";
                  end case;   
      
                  case Coeficiente is
                     when 0|2|3|9|10|20|21|35 =>
                        Columna := Columna(9 downto 3) & "000";
                     when 1|4|8|11|19|22|34|36 =>
                        Columna := Columna(9 downto 3) & "001";
                     when 5|7|12|18|23|33|37|48 =>
                        Columna := Columna(9 downto 3) & "010";
                     when 6|13|17|24|32|38|47|49 =>
                        Columna := Columna(9 downto 3) & "011";
                     when 14|16|25|31|39|46|50|57 =>
                        Columna := Columna(9 downto 3) & "100";
                     when 15|26|30|40|45|51|56|58 =>
                        Columna := Columna(9 downto 3) & "101";
                     when 27|29|41|44|52|55|59|62 =>
                        Columna := Columna(9 downto 3) & "110";
                     when 28|42|43|53|54|60|61|63 =>
                        Columna := Columna(9 downto 3) & "111";
                  end case;
            end case;
            end if;   
            if Done = '1' then
               HeaderFinal := '0';
               Done <= '0';
               --if LineAbsToCompress > ImgLines then --must process the last three luminance blocks one after the other
               if (Elemento = "00" and Linea(3) = '1' and Columna(3)='1' and StepV /= 0) or Bloque /= "00" then --only if the 4 squares of luminance 2x2 block have been processed
                  --when Elemento is 00 but Linea and Columna still are the ones of element 11
                  Linea := "0000";
                  if LumaBlock = '1' then --must change columns (it'll always be Elemento=11 because it is the only one that rises Done)
                     Columna := '0' & Columna(9 downto 4) & "000"; --half of the columns because of subsampling
                  else
                     Columna := Columna(9 downto 3) & "000";
                  end if;   
                  LumaBlock := '0';
                  StepV <= 0;
                  weY2 <= '0';
                  weCb2 <= '0';
                  weCr2 <= '0';
                  Coeficiente := 0;
                  Save <= '0';
                  IniDC := '1'; 
                  GetPrevDC := '1';
                  
                  case Bloque is
                     when "00" => --block Y
                        Bloque := "01"; --pass on to process las Cb block
                     when "01" => --block Cb
                        Bloque := "10"; --pass on to process las Cr block
                     when others => --can only be block Cr
                        CompressingInt <= '0'; --we've finished the block
                  end case;                        
               else
                  CompressingInt <= '0'; --we've finished compressing this block
               end if;
            end if;

            case Bloque is
               when "00" => --block Y
                  addrY2 <= Mult_Columns(Linea) + Columna;
               when "01" => --block Cb
                  addrCb2 <= Mult_Half_Columns(Linea(2 downto 0)) + Columna;
               when others => --can only be block Cr
                  addrCr2 <= Mult_Half_Columns(Linea(2 downto 0)) + Columna;
            end case;

         end if; --if MakeDCT or CompressingInt
      end if; --if clk
   end process JPEG;
end JPG;

configuration configuracionJPG of Compressor is
   for JPG 
      
      
--pragma translate_off
   for all : dct2d use entity XilinxCoreLib.C_DA_2D_DCT_V2_0(behavioral)
      generic map(
			c_clks_per_sample => 9,
			c_result_width => 19,
			c_internal_width => 19,
			c_data_type => 0,
			c_precision_control => 2,
			c_data_width => 8,
			c_operation => 0,
			c_enable_rlocs => 0,
			c_latency => 95,
			c_enable_symmetry => 1,
			c_coeff_width => 24,
			c_shape => 0,
			c_mem_type => 1,
			c_col_latency => 15,
			c_row_latency => 15,
			c_has_reset => 0);
   end for;
   
	for all : buffer_comp use entity XilinxCoreLib.blkmemsp_v5_0(behavioral)
      generic map(
			c_sinit_value => "0",
			c_reg_inputs => 0,
			c_yclk_is_rising => 1,
			c_has_en => 0,
			c_ysinit_is_high => 1,
			c_ywe_is_high => 1,
			c_ytop_addr => "1024",
			c_yprimitive_type => "16kx1",
			c_yhierarchy => "hierarchy1",
			c_has_rdy => 0,
			c_has_limit_data_pitch => 0,
			c_write_mode => 1,
			c_width => 12,
			c_yuse_single_primitive => 0,
			c_has_nd => 0,
			c_enable_rlocs => 0,
			c_has_we => 1,
			c_has_rfd => 0,
			c_has_din => 1,
			c_ybottom_addr => "0",
			c_pipe_stages => 0,
			c_yen_is_high => 1,
			c_depth => 5632,
			c_has_default_data => 1,
			c_limit_data_pitch => 18,
			c_has_sinit => 0,
			c_mem_init_file => "mif_file_16_1",
			c_default_data => "0",
			c_ymake_bmm => 0,
			c_addr_width => 13);
      end for;

	   for all : buffer_comp_chrom use entity XilinxCoreLib.blkmemsp_v5_0(behavioral)
         generic map(
			c_sinit_value => "0",
			c_reg_inputs => 0,
			c_yclk_is_rising => 1,
			c_has_en => 0,
			c_ysinit_is_high => 1,
			c_ywe_is_high => 1,
			c_ytop_addr => "1024",
			c_yprimitive_type => "16kx1",
			c_yhierarchy => "hierarchy1",
			c_has_rdy => 0,
			c_has_limit_data_pitch => 0,
			c_write_mode => 1,
			c_width => 12,
			c_yuse_single_primitive => 0,
			c_has_nd => 0,
			c_enable_rlocs => 0,
			c_has_we => 1,
			c_has_rfd => 0,
			c_has_din => 1,
			c_ybottom_addr => "0",
			c_pipe_stages => 0,
			c_yen_is_high => 1,
			c_depth => 1408,
			c_has_default_data => 1,
			c_limit_data_pitch => 18,
			c_has_sinit => 0,
			c_mem_init_file => "mif_file_16_1",
			c_default_data => "0",
			c_ymake_bmm => 0,
			c_addr_width => 11);
      end for;   
      
      for all : q_rom use entity XilinxCoreLib.blkmemsp_v5_0(behavioral)
         generic map(
			c_sinit_value => "0",
			c_reg_inputs => 0,
			c_yclk_is_rising => 1,
			c_has_en => 0,
			c_ysinit_is_high => 1,
			c_ywe_is_high => 1,
			c_ytop_addr => "1024",
			c_yprimitive_type => "16kx1",
			c_yhierarchy => "hierarchy1",
			c_has_rdy => 0,
			c_has_limit_data_pitch => 0,
			c_write_mode => 0,
			c_width => 13,
			c_yuse_single_primitive => 0,
			c_has_nd => 0,
			c_enable_rlocs => 0,
			c_has_we => 0,
			c_has_rfd => 0,
			c_has_din => 0,
			c_ybottom_addr => "0",
			c_pipe_stages => 0,
			c_yen_is_high => 1,
			c_depth => 384,
			c_has_default_data => 0,
			c_limit_data_pitch => 18,
			c_has_sinit => 0,
			c_mem_init_file => "q_rom.mif",
			c_default_data => "0",
			c_ymake_bmm => 0,
			c_addr_width => 9);
      end for;
      
      for all : huff_rom use entity XilinxCoreLib.blkmemsp_v5_0(behavioral)
         generic map(
			c_sinit_value => "0",
			c_reg_inputs => 0,
			c_yclk_is_rising => 1,
			c_has_en => 0,
			c_ysinit_is_high => 1,
			c_ywe_is_high => 1,
			c_ytop_addr => "1024",
			c_yprimitive_type => "16kx1",
			c_yhierarchy => "hierarchy1",
			c_has_rdy => 0,
			c_has_limit_data_pitch => 0,
			c_write_mode => 0,
			c_width => 20,
			c_yuse_single_primitive => 0,
			c_has_nd => 0,
			c_enable_rlocs => 0,
			c_has_we => 0,
			c_has_rfd => 0,
			c_has_din => 0,
			c_ybottom_addr => "0",
			c_pipe_stages => 0,
			c_yen_is_high => 1,
			c_depth => 352,
			c_has_default_data => 0,
			c_limit_data_pitch => 18,
			c_has_sinit => 0,
			c_mem_init_file => "huff_rom.mif",
			c_default_data => "0",
			c_ymake_bmm => 0,
			c_addr_width => 9);
      end for;   

	   for all : tabla_q use entity XilinxCoreLib.blkmemsp_v5_0(behavioral)
         generic map(
   			c_sinit_value => "0",
   			c_reg_inputs => 0,
   			c_yclk_is_rising => 1,
   			c_has_en => 0,
   			c_ysinit_is_high => 1,
   			c_ywe_is_high => 1,
   			c_ytop_addr => "1024",
   			c_yprimitive_type => "16kx1",
   			c_yhierarchy => "hierarchy1",
   			c_has_rdy => 0,
   			c_has_limit_data_pitch => 0,
   			c_write_mode => 0,
   			c_width => 8,
   			c_yuse_single_primitive => 0,
   			c_has_nd => 0,
   			c_enable_rlocs => 0,
   			c_has_we => 0,
   			c_has_rfd => 0,
   			c_has_din => 0,
   			c_ybottom_addr => "0",
   			c_pipe_stages => 0,
   			c_yen_is_high => 1,
   			c_depth => 384,
   			c_has_default_data => 0,
   			c_limit_data_pitch => 18,
   			c_has_sinit => 0,
   			c_mem_init_file => "tabla_q.mif",
   			c_default_data => "0",
   			c_ymake_bmm => 0,
   			c_addr_width => 9);
         end for;   
    --pragma translate_on

   end for;
end configuracionJPG;
