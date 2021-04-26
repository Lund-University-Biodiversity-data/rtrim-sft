CREATE DATABASE "rtrim-params";

\c "rtrim-params";

CREATE TYPE sftschemes AS ENUM ('totalsommar_pkt', 'totalstandard', 'totalvinter_pkt');

CREATE TABLE species_from_ala (
	id serial PRIMARY KEY,
	species_id varchar(3) NOT NULL UNIQUE,
	species_latin_name varchar(75),
	species_sw_name varchar(75),
	species_en_name varchar(75),
	species_worldname varchar(75),
	species_rank integer NOT NULL,
	species_guid integer NOT NULL
);

CREATE TABLE species_start_year (
	id serial PRIMARY KEY,
	species_id char(3) NOT NULL,
	species_sw_name varchar(50) NOT NULL,
	scheme sftschemes NOT NULL,
	year integer,
	comment TEXT
);

CREATE TABLE species_limit_north_south (
	id serial PRIMARY KEY,
	species_id char(3) NOT NULL,
	species_id_main char(3),
	species_sw_name varchar(50) NOT NULL,
	species_latin_name varchar(50) NOT NULL,
	species_en_name varchar(50) NOT NULL,
	latitude_limit varchar(20) 
);

INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('002', 'Smålom', 'totalsommar_pkt', 1975, 'Excl 1986, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('004', 'Gråhakedopping', 'totalsommar_pkt', 1992, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('008', 'Storskarv', 'totalsommar_pkt', 1985, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('010', 'Rördrom', 'totalsommar_pkt', 1976, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('015', 'Snatterand', 'totalsommar_pkt', 2004, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('016', 'Bläsand', 'totalsommar_pkt', 1983, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('018', 'Skedand', 'totalsommar_pkt', 1986, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('021', 'Brunand', 'totalsommar_pkt', 1980, 'Excl 2013, 2015, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('031', 'Grågås', 'totalsommar_pkt', 1981, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('036', 'Sångsvan', 'totalsommar_pkt', 1988, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('042', 'Duvhök', 'totalsommar_pkt', 1984, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('043', 'Röd glada', 'totalsommar_pkt', 1982, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('052', 'Lärkfalk', 'totalsommar_pkt', 1992, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('057', 'Tornfalk', 'totalsommar_pkt', 1987, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('061', 'Tjäder', 'totalsommar_pkt', 1984, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('062', 'Järpe', 'totalsommar_pkt', 1984, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('109', 'Dvärgmås', 'totalsommar_pkt', 1996, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('115', 'Silvertärna', 'totalsommar_pkt', 1975, 'Excl 1983, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('116', 'Småtärna', 'totalsommar_pkt', 1992, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('154', 'Trädlärka', 'totalsommar_pkt', 1975, 'Excl 1990, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('200', 'Kärrsångare', 'totalsommar_pkt', 1976, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('216', 'Halsbandsflugsnappare', 'totalsommar_pkt', 1997, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('225', 'Forsärla', 'totalsommar_pkt', 1985, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('231', 'Stenknäck', 'totalsommar_pkt', 1977, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('233', 'Steglits', 'totalsommar_pkt', 1983, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('243', 'Mindre korsnäbb', 'totalsommar_pkt', 2001, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('248', 'Bergfink', 'totalsommar_pkt', 1980, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('252', 'Videsparv', 'totalsommar_pkt', 1985, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('536', 'Gråsiska (S)', 'totalsommar_pkt', 1975, 'Excl 1978, 1979, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('636', 'Gråsiska (N)', 'totalsommar_pkt', 1984, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('488', 'Brandkronad kungsfågel', 'totalstandard', 2013, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('136', 'Hornuggla', 'totalstandard', 2008, 'Excl 2016');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('132', 'Sparvuggla', 'totalstandard', 2008, 'Excl 2015');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('139', 'Nattskärra', 'totalstandard', 2008, 'Excl 2013');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('072', 'Rörhöna', 'totalstandard', 2008, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('096', 'Skärsnäppa', 'totalstandard', 2007, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('134', 'Slaguggla', 'totalstandard', 2007, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('029', 'Salskrake', 'totalstandard', 2006, 'Excl 2014');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('116', 'Småtärna', 'totalstandard', 2006, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('015', 'Snatterand', 'totalstandard', 2004, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('018', 'Skedand', 'totalstandard', 2004, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('099', 'Myrsnäppa', 'totalstandard', 2004, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('131', 'Hökuggla', 'totalstandard', 2002, 'Excl 2012');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('005', 'Svarthakedopping', 'totalstandard', 2002, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('102', 'Smalnäbbad simsnäppa', 'totalstandard', 2002, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('109', 'Dvärgmås', 'totalstandard', 2002, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('242', 'Tallbit', 'totalstandard', 2002, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('004', 'Gråhakedopping', 'totalstandard', 2001, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('048', 'Blå kärrhök', 'totalstandard', 2001, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('052', 'Lärkfalk', 'totalstandard', 2001, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('067', 'Vattenrall', 'totalstandard', 2001, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('225', 'Forsärla', 'totalstandard', 2001, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('246', 'Bändelkorsnäbb', 'totalstandard', 2001, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('024', 'Svärta', 'totalstandard', 2000, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('025', 'Sjöorre', 'totalstandard', 2000, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('064', 'Vaktel', 'totalstandard', 2000, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('097', 'Mosnäppa', 'totalstandard', 2000, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('098', 'Kärrsnäppa', 'totalstandard', 2000, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('137', 'Jorduggla', 'totalstandard', 2000, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('204', 'Höksångare', 'totalstandard', 2000, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('255', 'Lappsparv', 'totalstandard', 2000, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('045', 'Havsörn', 'totalstandard', 1999, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('071', 'Kornknarr', 'totalstandard', 1999, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('079', 'Fjällpipare', 'totalstandard', 1999, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('033', 'Sädgås', 'totalstandard', 1998, 'Excl 2005');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('133', 'Kattuggla', 'totalvinter_pkt', 1975, 'Excl 2013, 2015, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('248', 'Bergfink', 'totalvinter_pkt', 1975, 'Excl 2003, 2010, 2012, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('230', 'Stare', 'totalvinter_pkt', 1975, 'Excl 1988, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('242', 'Tallbit', 'totalvinter_pkt', 1975, 'Excl 1984, 1986, 1994, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('145', 'Gråspett', 'totalvinter_pkt', 1975, 'Excl 1983, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('150', 'Tretåig hackspett', 'totalvinter_pkt', 1975, 'Excl 1983, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('048', 'Blå kärrhök', 'totalvinter_pkt', 1975, 'Excl 1981, 2010, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('053', 'Pilgrimsfalk', 'totalvinter_pkt', 2005, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('243', 'Mindre korsnäbb', 'totalvinter_pkt', 1998, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('244', 'Större korsnäbb', 'totalvinter_pkt', 1992, 'Excl 2010, som fick lägsta värdet i övrigt');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('016', 'Bläsand', 'totalvinter_pkt', 1986, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('043', 'Röd glada', 'totalvinter_pkt', 1980, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('031', 'Grågås', 'totalvinter_pkt', 1990, '');
INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment)  VALUES ('428', 'Vitkindad gås', 'totalvinter_pkt', 1999, '');

INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('508', '208', 'Lövsångare (S)', 'Ph. t. trochilus', 'Willow warbler (S)', 'S om 62°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('509', '209', 'Gransångare (S)', 'Ph. c. collybita', 'Chiffchaff (S)', 'S om 60°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('526', '226', 'Gulärla (S)', 'Motacilla f. flava', 'Yellow wagtail (S)', 'S om 60°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('536', '236', 'Gråsiska (S)', 'Acanthis f. cabaret', 'Lesser redpoll (S)', 'S om 60°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('566', '166', 'Nötkråka (S)', 'Nucifraga c. caryocatactes', 'Nutcracker (S)', 'S om 60°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('608', '208', 'Lövsångare (N)', 'Ph. t. acredula', 'Willow warbler (N)', 'N om 62°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('609', '209', 'Gransångare (N)', 'Ph. c. abietinus', 'Chiffchaff (N)', 'N om 60°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('626', '226', 'Gulärla (N)', 'Motacilla f. thunbergi', 'Yellow wagtail (N)', 'N om 60°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('636', '236', 'Gråsiska (N)', 'Carduelis f. flammea', 'Common redpoll (N)', 'N om 60°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('666', '166', 'Nötkråka (N)', 'Nucifraga c. macrorhynchos', 'Nutcracker (N)', 'N om 60°C'); 
INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit)  VALUES ('645', '', 'Korsnäbb total', 'Loxia total', 'Crossbill (all)', ''); 


