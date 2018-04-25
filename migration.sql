CREATE DATABASE IF NOT EXISTS scrapper;
USE scrapper;

CREATE TABLE IF NOT EXISTS `volatile`( 
	`id` int AUTO_INCREMENT, 
	`date` varchar(255), 
	`symbol` varchar(255), 
	`ucp` varchar(255),
	`updcp` varchar(255), 
	`ulr` varchar(255), 
	`pduv` varchar(255), 
	`cdudv` varchar(255), 
	`uav` varchar(255), 
	 PRIMARY KEY (id) 
 );