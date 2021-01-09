unit DuaneCrapsFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    FileNameEdt: TEdit;
    OkBtn: TButton;
    procedure OkBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses IOUtils, U494Util, SrcFile, ObjFile;

{$R *.dfm}

procedure TForm3.OkBtnClick(Sender: TObject);
var
    ifname, ofname: String;
    line: AnsiString;
    addr, itemp: UInt32;
    first: Boolean;
    tokens: TStringList;
    ifile: TSrcFileStream;
    ofile: TMemImageStream;
begin
    first := True;
    tokens := TStringList.Create;
    tokens.Delimiter := ' ';
    tokens.StrictDelimiter := False;
    ifname := FileNameEdt.Text;
    ifile := TSrcFileStream.Create(ifname);
    try
        ofname := TPath.GetDirectoryName(ifname) + '\' +
                  TPath.GetFileNameWithoutExtension(ifname) + '.mem';
        ofile := TMemImageStream.Create(ofName, fmCreate);
        try
            // Burn 1st 2 lines
            line := ifile.ReadLine;
            line := ifile.ReadLine;
            while (not ifile.Eof) do
            begin
                line := ifile.ReadLine;
                if (Trim(line) = '') then
                    Continue;
                tokens.DelimitedText := String(line);
                addr := Octal(tokens[0]);
                itemp := Octal(tokens[2] + tokens[3]);
                if (first) then
                begin
                    ofile.EmitTransferAddr(addr, addr, 0);
                    first := False;
                end;
                ofile.EmitSingleWord(addr, rtNone, itemp);
            end;
        finally
            ofile.Free;
        end;
    finally
        tokens.Free;
        ifile.Free;
    end;
end;

end.
