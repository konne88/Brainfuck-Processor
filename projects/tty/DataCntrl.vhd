-------------------------------------------------------------------------
-- stty -F /dev/ttyUSB0 9600 parenb parodd cstopb
-------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity DataCntrl is
	port ( 
        TXD     : out std_logic := '1';
        RXD     : in std_logic 	:= '1';
        CLK     : in std_logic;
        LEDS    : out std_logic_vector(7 downto 0) := "11111111";
--        RST     : in std_logic	:= '0'

        sri_adr  : out std_logic_vector(23 downto 1);
        sri_data : inout std_logic_vector(15 downto 0);
        sri_oe   : out std_logic;
        sri_we   : out std_logic;
        sri_cs    : out std_logic;
        sri_lb   : out std_logic;
        sri_ub   : out std_logic;
        sri_adv  : out std_logic;
        sri_clk  : out std_logic;
        sri_cre   : out std_logic := '0';
		  sri_wait : in std_logic
	);
end DataCntrl;

architecture Behavioral of DataCntrl is
    component Fifo
		  port (
			  clk         : in std_logic;
			  reset	     : in std_logic;
			  datain        : inout std_logic_vector(7 downto 0);
			  dataout        : inout std_logic_vector(7 downto 0);
			  empty       : out std_logic := '1';
	        full        : out std_logic := '0';
	        enabled     : in std_logic := '0';
			  push        : in std_logic
		  );
    end component;

    component RS232RefComp
        port (  	
            TXD 	: out	std_logic	:= '1';
            RXD 	: in	std_logic;					
            CLK 	: in	std_logic;							
            DBIN 	: in	std_logic_vector (7 downto 0);
            DBOUT   : out	std_logic_vector (7 downto 0);
            RDA	    : inout	std_logic;							
            TBE	    : inout	std_logic 	:= '1';				
            RD		: in	std_logic;							
            WR		: in	std_logic;							
            PE		: out	std_logic;							
            FE		: out	std_logic;							
            OE		: out	std_logic;											
            RST	    : in	std_logic
        );				
    end component;	

-------------------------------------------------------------------------
--
--Title:	Type Declarations
--
--Description:	There is one state machine used in this program, called 
--				the mainState state machine.  This state machine controls 
--				the flow of data around the UART; allowing for data to be
--				changed from serial to parallel, and then back to serial.
--
-------------------------------------------------------------------------
--	type mainState is (
	--	stReceive,
		--stSend);
-------------------------------------------------------------------------
--
--Title:  Local Signal Declarations
--
--Description:	The signals used by this entity are described below:
--
--				-dbInSig 	:  	This signal is the parallel data input  
--								for the UART
--				-dbOutSig	:	This signal is the parallel data output 
--								for the UART
--      		-rdaSig		:	This signal will get the RDA signal from 
--								the UART
--			 	-tbeSig		:	This signal will get the TBE signal from 
--								the UART
-- 				-rdSig		:	This signal is the RD signal for the UART
-- 				-wrSig		:	This signal is the WR signal for the UART
-- 				-peSig		:	This signal will get the PE signal from 
--								the UART
-- 				-feSig		:	This signal will get the FE signal from 
--								the UART
-- 				-oeSig		:	This signal will get the OE signal from 
--								the UART
--
--				The following signals are used by the main state machine
--				for state control:
--				
--				-stCur, stNext	
--	
-------------------------------------------------------------------------
	signal dbInSig	:	std_logic_vector(7 downto 0);
	signal dbOutSig :	std_logic_vector(7 downto 0);
	signal rdaSig	:	std_logic;
	signal tbeSig	:	std_logic;
	signal rdSig	:	std_logic;
	signal wrSig	:	std_logic;
	signal peSig	:	std_logic;
	signal feSig	:	std_logic;
	signal oeSig	:	std_logic;
    
    signal fifoIn  : std_logic_vector(7 downto 0);
    signal fifoOut  : std_logic_vector(7 downto 0);
    signal fifoEmpty : std_logic;
    signal fifoFull  : std_logic;
--    signal fifoAddr  : std_logic_vector(23 downto 0);
    signal fifoReset : std_logic;
    signal fifoPush  : std_logic;
    signal fifoEnabled : std_logic := '0';
    
	 signal rstSig : std_logic := '0';
--    signal noAddr  : std_logic_vector(23 downto 0);
    
--	 signal byteSelect : std_logic;
--    signal slowClk : std_logic_vector(25 downto 0);


	type ram_states is (
		ioselect, 
	
		write_00,
		write_01, 
		write_02, 
		write_03,
		write_04,
		write_05,
		
		read_00,
		read_01, 
		read_02,
		read_03,
		read_04,
		read_05,
		
		wait_for_stb_neg
	);

	signal ram_state	: ram_states;
	signal sram_gce	: std_logic;
	signal ri_rdy : std_logic;
begin
	 ttyfifo : Fifo port map (	
       clk    => CLK,
       reset  => fifoReset,
       datain   => fifoIn,
	    dataout  => fifoOut,
       empty  => fifoEmpty,
       full   => fifoFull,
	    enabled => fifoEnabled,
	    push => fifoPush
    );

	 tty : RS232RefComp port map (	
        TXD    => TXD,
        RXD    => RXD,
        CLK    => CLK,
        DBIN   => dbInSig,
        DBOUT  => dbOutSig,
        RDA	   => rdaSig,
        TBE	   => tbeSig,	
        RD	   => rdSig,
        WR	   => wrSig,
        PE	   => peSig,
        FE	   => feSig,
        OE	   => oeSig,
        RST    => rstSig
    );

--    MEMDATA (15 downto 8) <= MEMDATA (7 downto 0);
--   MEMADDR (23 downto 0) <= "000000000000000000000000";
--	 MEMDATA (7 downto 0) <= "00000000";
	 
	 fifoReset <= '0';
	 rstSig <= '0';
--	 
--	 LEDS(7) <= '0';	 
--	 LEDS(2) <= fifoEmpty;
--	 LEDS(1) <= tbeSig;
--	 LEDS(0) <= rdaSig;
--
--	process (CLK)
--		variable oe : std_logic := '0';
--		variable fe : std_logic := '0';
--		variable pe : std_logic := '0';
--		variable ff : std_logic := '0';
--	begin	
--		LEDS(6) <= oe;
--		LEDS(5) <= fe;
--		LEDS(4) <= pe;
--		LEDS(3) <= ff;
--		
--		if (CLK = '1' and CLK'Event) then
--			if (oeSig = '1') then
--				oe := '1';
--			end if;
--			if (feSig = '1') then
--				fe := '1';
--			end if;
--			if (peSig = '1') then
--				pe := '1';
--			end if;
--			if (fifoFull = '1') then
--				ff := '1';
--			end if;
--		end if;
--	end process;


--    ROMCLK <= CLK;
--    ROMCE  <= '1';
--    ROMCRE <= '0';
--	 ROMWAIT <= '0';
	 
--	 MEMOE <= '0';
--	 MEMWE <= '0';
	 
   -- byteSelect <= '0';
    
--	process (CLK) 
--		
--	begin
--		
--		if (CLK = '1' and CLK'Event) then
--			if rdaSig = '1' then
--			end if;
--		end if;
--	end process;


	process (CLK)
		variable state : std_logic_vector(2	downto 0) := "000";
		variable buff : std_logic_vector(7	downto 0) := "00000000";
		variable recved : std_logic_vector(7 downto 0) := "00000000";
	begin
		dbInSig <=  fifoOut;
		fifoIn <= dbOutSig;
		
	--	LEDS(7 downto 0) <= recved;
		
		if (CLK = '1' and CLK'Event) then
			case state is
				when "000" =>
					fifoEnabled <= '0';
					fifoPush <= '0';	
					wrSig <= '0';
					rdSig <= '0';
					if rdaSig = '1' and fifoFull = '0' then
						state := "001";	-- reading
					elsif tbeSig = '1' and  fifoEmpty = '0' then
						state := "100";	-- writing
						recved := recved + 1;
					end if;
				when "001" =>
					fifoEnabled <= '1';
					fifoPush <= '1';	
					wrSig <= '0';
					rdSig <= '0';
					state := "010";
				when "010" =>
					fifoEnabled <= '0';
					fifoPush <= '0';
					wrSig <= '0';
					rdSig <= '0';
					state := "011";
				when "011" =>
					fifoEnabled <= '0';
					fifoPush <= '0';
					wrSig <= '0';
					rdSig <= '1';
					state := "111";
				when "100" =>
					fifoEnabled <= '1';
					fifoPush <= '0';
					wrSig <= '0';
					rdSig <= '0';
					state := "101";
				when "101" => 
					fifoEnabled <= '0';
					fifoPush <= '0';
					wrSig <= '0';
					rdSig <= '0';
					state := "110";
				when "110" =>
					fifoEnabled <= '0';
					fifoPush <= '0';
					rdSig <= '0';
					wrSig <= '1';
					state := "111";
				when "111" =>
					fifoEnabled <= '0';
					fifoPush <= '0';
					wrSig <= '0';
					rdSig <= '0';
				when others => 
					null;
			end case;
		end if;
	end process;
	
	
--	ROMWAIT <= '0';
		 	 
--    byteSelect <= '0';
    
--    ROMLB  <= byteSelect;
--	 ROMUB  <= not byteSelect;
--
--	ROMADV <= '0';
--	ROMCRE <= '1';
--
--	process (CLK)
--		variable state : std_logic_vector(3 downto 0):= "0000";
--		variable output : std_logic_vector(15 downto 0) := "0000000000000000";
--		variable sleep : std_logic_vector(25 downto 0) := "00000000000000000000000000";
--		variable a : std_logic;
--		variable b : std_logic;
--		variable varnot : std_logic := '1';
--	begin
--		LEDS(6 downto 0) <= output(6 downto 0);
--	
--		LEDS(7) <= ROMWAIT;
--		ROMCE <= varnot xor '0';
--	
--		if (CLK = '1' and CLK'Event) then
--			if sleep /= "00000000000000000000000000" then
--				sleep := sleep + 1;
--			else
--				case state is
--					when "0000" =>
--						MEMADDR <= "00000000000000000000001";
--						MEMDATA <= "1001100101100110";
--						MEMOE <= varnot xor  '0';
--						MEMWE <= varnot xor  '1';
--						state := "0001";
--					when "0001" =>
--						MEMADDR <= "00000000000000000000010";
--						MEMDATA <= "1011100101110110";
--						MEMOE <= varnot xor  '0';
--						MEMWE <= varnot xor  '1';
--						state := "0010";
--					when "0010" =>
--						MEMADDR <= "00000000000000000000011";
--						MEMDATA <= "1111100101111010";
--						MEMOE <= varnot xor  '0';
--						MEMWE <= varnot xor  '1';
--						state := "0011";
--					when "0011" =>
--						MEMADDR <= "00000000000000000000001";
--						MEMOE <= varnot xor  '1';
--						MEMWE <= varnot xor  '0';
--						state := "0100";
--					when "0100" =>
--						output := MEMDATA;
--						MEMADDR <= "00000000000000000000001";
--						MEMOE <= varnot xor  '1';
--						MEMWE <= varnot xor  '0';
--						state := "0000";
--					when others =>
--						null;
--				end case;
--				sleep := "00000000000000000000000001";
--			end if;
--		end if;
--	end process;
--

























	sri_ub 	<= sram_gce;
	sri_lb 	<= sram_gce;
	sri_cs   <= sram_gce;
	-- sri_adr <= ri_adr;
	sri_clk		<= '0';
	sri_cre     <= '0';

	process (clk)
		variable state : std_logic_vector(1 downto 0) := "00";
		variable ledbuf : std_logic_vector(15 downto 0);
	begin
		LEDS(5 downto 0) <= ledbuf(5 downto 0);
		LEDS(6) <= sri_wait;
		LEDS(7) <= ledbuf(7);

	
--		if res = '1' then
--			ram_state   <= ioselect;			
--			sri_data 	<= (others => 'Z');
--			sri_adr 	   <= (others => '0');
--			sri_oe 		<= '1';
--			sram_gce 	<= '1';
--			sri_we 	 	<= '1';
--			sri_adv		<= '1';
--		els
		if rising_edge(clk) then
			case ram_state is
				when ioselect =>
					sri_adr(23 downto 2) <= (others => '0');
					case state is
						when "00" =>
							sri_adr(1) <= '0';
						when "01" =>
							sri_adr(1) <= '1';
						when "10" =>
							sri_adr(1) <= '0';
							when others =>
														null;
					end case;
					sri_adv <= '0';
								
					if state = "10" then
						ram_state	<= read_00;
						sri_data		<= (others => '0');
						sram_gce		<= '1';
						sri_oe		<= '1';
						sri_we		<= '1';
					else
						ram_state	<= write_00;
						sri_data		<= (others => '0');
						sram_gce 	<= '0';
						sri_oe		<= '1';
						sri_we		<= '0';
					end if;
				when write_00 	=>			
												case state is
													when "00" =>
														sri_data <=  "0000000000101001";
													when "01" =>
														sri_data <=  "0000000000000110";
												when others =>
														null;
												end case;
												sram_gce	<= '0';
												sri_oe  	<= '1';
												sri_we  	<= '0';
												ri_rdy	<= '0';
												ram_state	<= write_01;

				when write_01 	=>  
												sram_gce	<= '0';
												sri_oe	<= '1';
												sri_we	<= '0';
												ri_rdy	<= '0';
												ram_state	<= write_02;
															

				when write_02 	=>
												sram_gce <= '0';
												sri_oe  	<= '1';
												sri_we  	<= '0';
												ri_rdy 	<= '0';
												ram_state	<= write_03;


				when write_03 	=>
												sram_gce	<= '0';
												sri_oe	<= '1';
												sri_we	<= '0';
												ri_rdy	<= '0';
												ram_state <= write_04;															

				when write_04 	=>
												sram_gce	<= '1';
												sri_oe  	<= '1';
												sri_we  	<= '0';
												ri_rdy 	<= '0';
												ram_state <= write_05;			

				when write_05 	=>  		sri_data	<= (others => 'Z');
												sram_gce	<= '1';
												sri_oe  	<= '1';
												sri_we  	<= '1';
												ri_rdy 	<= '1';
												ram_state <= ioselect;
												case state is
													when "00" =>
														state := "01";
													when "01" =>
														state := "10";
													when others =>
														null;
												end case;



				when read_00	=>
												sri_data	<= (others => 'Z');
												sram_gce	<= '0';
												sri_oe	<= '1';
												sri_we	<= '1';
												ri_rdy	<= '0';
												ram_state <= read_01;

				when read_01 	=>
                              
												sram_gce	<= '0';
												sri_oe	<= '0';
												sri_we	<= '1';
												ri_rdy	<= '0';
												ram_state <= read_02;

				when read_02 	=>
												sram_gce	<= '0';
												sri_oe	<= '0';
												sri_we	<= '1';
												ri_rdy	<= '0';
												ram_state <= read_03;
															
				when read_03 	=>
												sram_gce	<= '0';
												sri_oe	<= '0';
												sri_we	<= '1';
												ri_rdy	<= '0';
												ram_state <= read_04;


				when read_04 	=>		
												ledbuf	:= sri_data;
												sram_gce	<= '0';
												sri_oe	<= '0';
												sri_we	<= '1';
												ri_rdy	<= '0';
												ram_state <= read_05;	
															
				when read_05 	=>
												sram_gce	<= '1';
												sri_oe	<= '1';
												sri_we	<= '1';
												ri_rdy	<= '1';
												ram_state <= ioselect;	
												state := "00";
	
				when others 	=>			ram_state	<= ioselect;
												ri_rdy		<= '0';
			end case;
			null;
		end if;
	end process;








	
--	process (clk) 
--	begin
--		if(clk'event and clk = '1') then
--			case 
--				when bfWrite =>
--					
--				when bfRead =>
--					
--				when bfNext =>
--					
--				when bfPrev =>
--					
--				when bfAdd =>
--					
--				when bfSub =>
--					
--				when bfLoopStart =>
--					
end Behavioral;

