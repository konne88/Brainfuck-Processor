----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    12:30:41 05/02/2010 
-- Design Name: 
-- Module Name:    BufferedSerialIO - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

---- Uncomment the following library declaration if instantiating
---- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity BufferedSerialIO is
    Port (
		  clk          : in std_logic;
        reset	      : in std_logic;
        datain       : in std_logic_vector(7 downto 0);
		  dataout      : out std_logic_vector(7 downto 0);
        full         : out std_logic := '0';
        enabled      : in std_logic := '0';
		  push         : in std_logic := '1';
		  ready		   : out std_logic := '0';
		  writedone		: out std_logic := '1';
	);
end BufferedSerialIO;

architecture Behavioral of BufferedSerialIO is

begin


end Behavioral;

