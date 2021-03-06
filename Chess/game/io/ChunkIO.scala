package game.io

import java.io.IOException
import java.io.Reader
import game.Board
import game.Game
import scala.collection.mutable.Buffer

/**********************************************************************
 *
 *   This file is returned in chapter 15.4, in exercises
 *   ChunkIO 1, 2 and 3.
 *
 *   The idea is to read a file written automatically by
 *   a program. This is a chunked format like PNG, TIF, MPEG, etc.
 *   explained in the exercise description
 *
 *   In ChunkIO 1, you only need to traverse through the file and extract
 *   the data portions of the Player block (PLR) and the Comment block (CMT).
 *   These are the characters that follow the chunk tag and the block size.
 *   E.g. for a player block "PLR17B5MarkoKa4Ra6b3c3" the data portion would be
 *   "B5MarkoKa4Ra6b3c3". Use the playerChunks and commentString variables inside
 *   loadGame to store the results.
 *
 *   In ChunkIO 2, you will implement a working version of the loadGame method.
 *   However the method only needs to work on correctly formatted files.
 *   Here you only need to handle the player blocks as they are the only thing
 *   that contains the game  information. You can use helper methods to separate
 *   the work into smaller subtasks and you can check the material for some suggestions.
 *
 *   ChunkIO 3 adds in the Unknown or Unused blocks and handling Broken Files.
 *
 *   You have to read through unknown blocks, but you don't have to interact with them further.
 *   If someone would want to update this application later with this feature, they could
 *   easily add a new block and simply add a feature to the code associated with that block
 *   without having to worry about whether the program can read bit or not.
 *
 *   For broken files, if the method encounters exceptions in the file structure, it should throw a
 *   CorruptedChessFileException. The material details what kind of problems you should
 *   expect so here you should adjust your code to deal with these erroneous situations. The broken files
 *   tests require that your program works also with correctly formatted input so doing
 *   exercise ChunkIO 2 before this one is advised.
 *
 *   For each exercise, you will always submit the same file, but each exercise has different requirements
 *   as described above and further in the materials, meaning that you only get points if you fulfil
 *   the requirements of that particular exercise.
 *
 *   Note:	ChunkIO 1 & 2 use correctly formatted files, so you have to worry about
 *   				broken files only in ChunkIO 3.
 *
 *
 **********************************************************************/

object ChunkIO {

    /**
     *
     */
	private var prevPlayerData = collection.mutable.Seq[String]()
	private var prevCommentData : Option[String] = None

	/**
	 * Processes a data file into a game object.
	 *
	 * @param input a reader (or stream) to a data file
	 */
    def loadGame(input: Reader): Game = {

        /**
         * This is the game object this method will fill with data. The object
         * is returned when the END chunk is reached.
         */

        val board = new Board()
        val game  = new Game(board)


        /*
         * Use these variables for reading all the file header, date and chunk headers.
         *
         * HINT: check the helper methods in the end of this class, a few lines below we read the header
         *       as an example
         */

        var header = new Array[Char](8)
        var date   = new Array[Char](8)
        var chunkHeader = new Array[Char](5)

        try {

            // Read the file header and the save date

            Helpers.readFully(header, input);
            Helpers.readFully(date, input);

            // Process the data we just read.
            // NOTE: To test the line below you must test the class once with a broken header

            if (!header.mkString.startsWith("CHESS")) {
                throw new CorruptedChessFileException("Unknown file type");
            }

            // The version information and the date are not used in this
            // exercise

           	// Store all of the (typically 2) player data chunks here for later inspection
           	val playerChunks = Buffer[String]()
       			//Store the comment block's (CMT) content here
      			var commentString: Option[String] = None

            while (chunkHeader.mkString != "END00") {
              var chars = new Array[Char](5)
              Helpers.readFully(chars, input)
              var data = new Array[Char](Helpers.extractChunkSize(chars))
              Helpers.readFully(data, input)
              Helpers.extractChunkName(chars) match {
                case "CMT" => commentString = Some(data.mkString)
                case "PLR" => playerChunks += data.mkString
                case "END" => chunkHeader = chars
              }
              chunkHeader = chars
            }

      			//ChunkIO 1
      			//These values are saved for testing that correct data chunk was read.
      			this.prevPlayerData = playerChunks
      			this.prevCommentData = commentString

            // ChunkIO 2 & 3
      			// If we reach this point the Game-object should now have the proper players and
            // a fully set up chess board. Therefore we might as well return it.

          // Is the number of players valid?

    if (playerChunks.length != 2) {

      throw new CorruptedChessFileException(s"Abnormal number of players, ${playerChunks.length} found")

    }

    for (chunk <- playerChunks) {
      val color = chunk(0) match {
        case 'W' => {
          if (game.getWhite.isDefined)
            throw new CorruptedChessFileException("Redefinion of white player")
          White
        }
        case 'B' => {
          if (game.getBlack.isDefined)
            throw new CorruptedChessFileException("Redefinion of black player")
          Black
        }
        case letter => throw new CorruptedChessFileException("Unknown player type: $letter")
      }

      val nameLength = chunk(1).toInt
      val name = chunk.slice(2, nameLength + 2)
      val player = new Player(name, color)
      game.addPlayer(player)

      val pieceData = chunk.drop(nameLength + 2)
      processPieces(pieceData, board, player)
    }

           game;

        } catch {
            case e:IOException =>


            // To test this part the stream would have to cause an
            // IOException. That's a bit complicated to test. Therefore we have
            // given you a "secret tool", class BrokenReader, which will throw
            // an IOException at a requested position in the stream.
            // Throw the exception inside any chunk, but not in the chunk header.

            val chessExc = new CorruptedChessFileException("Reading the chess data failed.")

            // Append the information about the initial cause for use in
            // debugging. Otherwise the programmer cannot know the method or
            // line number causing the problem.

            chessExc.initCause(e)

            throw chessExc
        }

    }

    /**
		 * Method used in testing to check that player data was read correctly
		 */
		def getPrevPlayerData = this.prevPlayerData

		/**
		 * Method used in testing to check that comment data was read correctly
		 */
		def getPrevCommentData = this.prevCommentData

    object Helpers {
    // HELPER METHODS -------------------------------------------------------

    /**
     * Given a chunk header (an array of 5 chars) will return the size of this
     * chunks data.
     *
     * @param chunkHeader
     *            a chunk header to process
     * @return the size of this chunks data
     */

    def extractChunkSize(chunkHeader: Array[Char]):Int = {
			10 * chunkHeader(3).asDigit + chunkHeader(4).asDigit
    }

    /**
     * Given a chunk header (an array of 5 chars) will return the name of this
     * chunk as a 3-letter String.
     *
     * @param chunkHeader
     *            a chunk header to process
     * @return the name of this chunk
     */
    def extractChunkName(chunkHeader: Array[Char]): String = {
        chunkHeader.take(3).mkString
    }

    /**
     * The read-method of the Reader class will occasionally read only part of
     * the characters that were requested. This method will repeatedly call read
     * to completely fill the given buffer. The size of the buffer tells the
     * algorithm how many bytes should be read.
     *
     * @param result
     *            The result of the reading will be stored in this array.
     * @param input
     *            The character stream to read from
     * @throws IOException
     * @throws CorruptedChessFileException
     */
    def readFully(result: Array[Char], input: Reader) = {
        var cursor = 0

        while (cursor != result.length) {
            var numCharactersRead = input.read(result, cursor, result.length - cursor)

            // If the file end is reached before the buffer is filled
            // an exception is thrown.

            if (numCharactersRead == -1) {
                throw new CorruptedChessFileException("Unexpected end of file.")
            }

            cursor += numCharactersRead
        }

    }

}

}
