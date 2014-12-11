# ErlyCounter is simple server for counters working via UDP protocol.

Server opens ports from A to B including to listen for received UDP packets.

Packet format is:

	type@identifier_counter0 -Step- \n
	
	type@identifier_counter1 -Step- \n
	
	type@identifier_counter2 -Step- \n
	
	type@identifier_counter3 -Step- \n

where
  -Step- step of increment counter identifier_counter (number as string)
  
  type - identifier of type of counter (designed to determine the storage location of counter within the program)
  
After a specified time, the server calls an external program and passes it (in stdin) accumulated ordered by the name of counter data in the format:

	type@identifier_counter -Value- \ n
	
	type@identifier_counter -Value- \ n
	
	type@identifier_counter -Value- \ n
	
	type@identifier_counter -Value- \ n
	
	type@identifier_counter -Value- \ n

where 
  
  -Value- - the accumulated data on the counter.

The data on the counters are cleared.

The counters of one type is transmitted to the external program at a time.  
  

# ErlyCounter - простой сервер счетчиков работающий через UDP-протокол.

Сервер открывает все порты с между A..B включительно, и слушает их на предмет прихода UDP-пакета.

Формат пакета:

	type@identifier_counter0 -Step- \n
	
	type@identifier_counter1 -Step- \n
	
	type@identifier_counter2 -Step- \n
	
	type@identifier_counter3 -Step- \n

где
	-Step- - шаг приращения счетчика identifier_counter (число в виде строки)

	type - идентификатор типа счетчика (предназначен для определения места хранения счетчика внутри программы).

Через заданное время, сервер вызывает внешнюю программу и передает ей (в stdin) накопившиеся упорядоченные по имени счетчиков данные в формате:

type@identifier_counter -Value- \n

type@identifier_counter -Value- \n

type@identifier_counter -Value- \n

type@identifier_counter -Value- \n

type@identifier_counter -Value- \n


где -Value- - накопленные данные о счетчике.

При этом данные о счетчиках стираются.

Счетчики одного типа передаются во внешнюю программу за один раз.

## Примеры
Задача 1: Логировать домен реферера, откуда пришел пользователь на сайт.

Решение: Отправлять UDP-пакет вида:

cntRefererDomain@<domain> 1 \n

где <domain> - реферер с которого пришел юзер (желательно в кодировке base64 чтобы не встречались пробелы)

Каждый период будет запускаться скрипт, который может обрабатывать как-то данные, например добавлять их в соотвествующу таблицу.
