--------------------------------------------------------------------------------
--     This file is owned and controlled by Xilinx and must be used           --
--     solely for design, simulation, implementation and creation of          --
--     design files limited to Xilinx devices or technologies. Use            --
--     with non-Xilinx devices or technologies is expressly prohibited        --
--     and immediately terminates your license.                               --
--                                                                            --
--     XILINX IS PROVIDING THIS DESIGN, CODE, OR INFORMATION "AS IS"          --
--     SOLELY FOR USE IN DEVELOPING PROGRAMS AND SOLUTIONS FOR                --
--     XILINX DEVICES.  BY PROVIDING THIS DESIGN, CODE, OR INFORMATION        --
--     AS ONE POSSIBLE IMPLEMENTATION OF THIS FEATURE, APPLICATION            --
--     OR STANDARD, XILINX IS MAKING NO REPRESENTATION THAT THIS              --
--     IMPLEMENTATION IS FREE FROM ANY CLAIMS OF INFRINGEMENT,                --
--     AND YOU ARE RESPONSIBLE FOR OBTAINING ANY RIGHTS YOU MAY REQUIRE       --
--     FOR YOUR IMPLEMENTATION.  XILINX EXPRESSLY DISCLAIMS ANY               --
--     WARRANTY WHATSOEVER WITH RESPECT TO THE ADEQUACY OF THE                --
--     IMPLEMENTATION, INCLUDING BUT NOT LIMITED TO ANY WARRANTIES OR         --
--     REPRESENTATIONS THAT THIS IMPLEMENTATION IS FREE FROM CLAIMS OF        --
--     INFRINGEMENT, IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS        --
--     FOR A PARTICULAR PURPOSE.                                              --
--                                                                            --
--     Xilinx products are not intended for use in life support               --
--     appliances, devices, or systems. Use in such applications are          --
--     expressly prohibited.                                                  --
--                                                                            --
--     (c) Copyright 1995-2003 Xilinx, Inc.                                   --
--     All rights reserved.                                                   --
--------------------------------------------------------------------------------
-- You must compile the wrapper file buffer_comp.vhd when simulating
-- the core, buffer_comp. When compiling the wrapper file, be sure to
-- reference the XilinxCoreLib VHDL simulation library. For detailed
-- instructions, please refer to the "CORE Generator Guide".

-- The synopsys directives "translate_off/translate_on" specified
-- below are supported by XST, FPGA Compiler II, Mentor Graphics and Synplicity
-- synthesis tools. Ensure they are correct for your synthesis tool(s).

-- synopsys translate_off
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

Library XilinxCoreLib;
ENTITY buffer_comp IS
	port (
	addr: IN std_logic_VECTOR(12 downto 0);
	clk: IN std_logic;
	din: IN std_logic_VECTOR(11 downto 0);
	dout: OUT std_logic_VECTOR(11 downto 0);
	we: IN std_logic);
END buffer_comp;

ARCHITECTURE buffer_comp_a OF buffer_comp IS

component wrapped_buffer_comp
	port (
	addr: IN std_logic_VECTOR(12 downto 0);
	clk: IN std_logic;
	din: IN std_logic_VECTOR(11 downto 0);
	dout: OUT std_logic_VECTOR(11 downto 0);
	we: IN std_logic);
end component;

-- Configuration specification 
	for all : wrapped_buffer_comp use entity XilinxCoreLib.blkmemsp_v5_0(behavioral)
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
BEGIN

U0 : wrapped_buffer_comp
		port map (
			addr => addr,
			clk => clk,
			din => din,
			dout => dout,
			we => we);
END buffer_comp_a;

-- synopsys translate_on

