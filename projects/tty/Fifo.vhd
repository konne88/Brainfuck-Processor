library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity Fifo is
    port (
        clk          : in std_logic;
        reset	      : in std_logic;
        datain       : in std_logic_vector(7 downto 0);
		  dataout      : out std_logic_vector(7 downto 0);
        empty        : out std_logic := '1';
        full         : out std_logic := '0';
        enabled      : in std_logic := '0';
		  push         : in std_logic := '1'
    );
end Fifo;
--
--architecture Behavioral of Fifo is
--begin
--	process (clk)
--        variable varFull  : std_logic := '0';
--        variable varEmpty : std_logic := '1';
--		  variable writeOffset : std_logic_vector(2 downto 0) := "000";
--        variable readOffset  : std_logic_vector(2 downto 0) := "000";
--        variable buff : std_logic_vector(63 downto 0);
--	begin
--		full <= varFull;
--		empty <= varEmpty;
--	
--		if(clk'event and clk = '1') then
--			if ( reset = '1') then
--				writeOffset := "000";
--				readOffset  := "000";
--				varFull := '0';
--				varEmpty := '1';
--			elsif ( enabled = '1' and push = '1' and varFull = '0') then	-- push
--				case writeOffset is
--					when "000" =>
--						buff(7 downto 0) := datain;
--					when "001" =>
--						buff(15 downto 8) := datain;
--					when "010" =>
--						buff(23 downto 16) := datain;
--					when "011" =>
--						buff(31 downto 24) := datain;
--					when "100" =>
--						buff(39 downto 32) := datain;
--					when "101" =>
--						buff(47 downto 40) := datain;
--					when "110" =>
--						buff(55 downto 48) := datain;
--					when "111" =>
--						buff(63 downto 56) := datain;
--					when others =>
--						null;
--				end case;
--				
--				writeOffset := writeOffset + 1;
--            varEmpty := '0';
--            if writeOffset = readOffset then
--					 varFull := '1';
--            else
--                varFull := '0';
--            end if;
--        elsif ( enabled = '1' and push = '0' and varEmpty = '0') then  -- pop
--				case readOffset is
--					when "000" =>
--						dataout <= buff(7 downto 0);
--					when "001" =>
--						dataout <= buff(15 downto 8);
--					when "010" =>
--						dataout <= buff(23 downto 16);
--					when "011" =>
--						dataout <= buff(31 downto 24);
--					when "100" =>
--						dataout <= buff(39 downto 32);
--					when "101" =>
--						dataout <= buff(47 downto 40);
--					when "110" =>
--						dataout <= buff(55 downto 48);
--					when "111" =>
--						dataout <= buff(63 downto 56);
--					when others =>
--						null;
--				end case;
--				
--				readOffset := readOffset + 1;
--            varFull := '0';
--            if writeOffset = readOffset then
--                varEmpty := '1';
--            else
--                varEmpty := '0';
--            end if;
--			end if;
--        end if;
--    end process;
--end Behavioral;


architecture Behavioral of Fifo is
begin
	process (clk)
        variable varFull  : std_logic := '0';
        variable varEmpty : std_logic := '1';
		  variable writeOffset : std_logic_vector(0 downto 0) := "0";
        variable readOffset  : std_logic_vector(0 downto 0) := "0";
        variable buff : std_logic_vector(63 downto 0);
	begin
		full <= varFull;
		empty <= varEmpty;
	
		if(clk'event and clk = '1') then
			if ( reset = '1') then
				writeOffset := "0";
				readOffset  := "0";
				varFull := '0';
				varEmpty := '1';
			elsif ( enabled = '1' and push = '1' and varFull = '0') then	-- push
				case writeOffset is
					when "0" =>
						buff(7 downto 0) := datain;
					when "1" =>
						buff(15 downto 8) := datain;
					when others =>
						null;
				end case;
				
				writeOffset := writeOffset + 1;
            varEmpty := '0';
            if writeOffset = readOffset then
					 varFull := '1';
            else
                varFull := '0';
            end if;
        elsif ( enabled = '1' and push = '0' and varEmpty = '0') then  -- pop
				case readOffset is
					when "0" =>
						dataout <= buff(7 downto 0);
					when "1" =>
						dataout <= buff(15 downto 8);
					when others =>
						null;
				end case;
				
				readOffset := readOffset + 1;
            varFull := '0';
            if writeOffset = readOffset then
                varEmpty := '1';
            else
                varEmpty := '0';
            end if;
			end if;
        end if;
    end process;
end Behavioral;

